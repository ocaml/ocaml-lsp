(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Cmarkit
module C = Cmarkit_renderer.Context
module String_set = Set.Make (String)

(* Renderer state *)

type state =
  { safe : bool;
    backend_blocks : bool;
    mutable ids : String_set.t;
    mutable footnote_count : int;
    mutable footnotes :
      (* Text, id, ref count, footnote *)
      (string * string * int ref * Block.Footnote.t) Label.Map.t  }

let state : state C.State.t = C.State.make ()
let safe c = (C.State.get c state).safe
let backend_blocks c = (C.State.get c state).backend_blocks
let init_context ?(backend_blocks = false) ~safe c _ =
  let ids = String_set.empty and footnotes = Label.Map.empty in
  let st = { safe; backend_blocks; ids; footnote_count = 0; footnotes } in
  C.State.set c state (Some st)

let unique_id c id =
  let st = C.State.get c state in
  let rec loop ids id c =
    let id' = if c = 0 then id else (String.concat "-" [id; Int.to_string c]) in
    match String_set.mem id' ids with
    | true -> loop ids id (c + 1)
    | false -> st.ids <- String_set.add id' ids; id'
  in
  loop st.ids id 0

let footnote_id label =
  let make_label l = String.map (function ' ' | '\t' -> '-' | c -> c) l in
  "fn-" ^ (make_label (String.sub label 1 (String.length label - 1)))

let footnote_ref_id fnid c = String.concat "-" ["ref"; Int.to_string c; fnid]

let make_footnote_ref_ids c label fn =
  let st = C.State.get c state in
  match Label.Map.find_opt label st.footnotes with
  | Some (text, id, refc, _) -> incr refc; (text, id, footnote_ref_id id !refc)
  | None ->
      st.footnote_count <- st.footnote_count + 1;
      let text = String.concat "" ["["; Int.to_string st.footnote_count;"]"] in
      let id = footnote_id label in
      st.footnotes <- Label.Map.add label (text, id, ref 1, fn) st.footnotes;
      text, id, footnote_ref_id id 1

(* Escaping *)

let buffer_add_html_escaped_uchar b u = match Uchar.to_int u with
| 0x0000 -> Buffer.add_utf_8_uchar b Uchar.rep
| 0x0026 (* & *) -> Buffer.add_string b "&amp;"
| 0x003C (* < *) -> Buffer.add_string b "&lt;"
| 0x003E (* > *) -> Buffer.add_string b "&gt;"
(* | 0x0027 (* ' *) -> Buffer.add_string b "&apos;" *)
| 0x0022 (* '\"' *) -> Buffer.add_string b "&quot;"
| _ -> Buffer.add_utf_8_uchar b u

let html_escaped_uchar c s = buffer_add_html_escaped_uchar (C.buffer c) s

let buffer_add_html_escaped_string b s =
  let string = Buffer.add_string in
  let len = String.length s in
  let max_idx = len - 1 in
  let flush b start i =
    if start < len then Buffer.add_substring b s start (i - start);
  in
  let rec loop start i =
    if i > max_idx then flush b start i else
    let next = i + 1 in
    match String.get s i with
    | '\x00' ->
        flush b start i; Buffer.add_utf_8_uchar b Uchar.rep; loop next next
    | '&' -> flush b start i; string b "&amp;"; loop next next
    | '<' -> flush b start i; string b "&lt;"; loop next next
    | '>' -> flush b start i; string b "&gt;"; loop next next
(*    | '\'' -> flush c start i; string c "&apos;"; loop next next *)
    | '\"' -> flush b start i; string b "&quot;"; loop next next
    | c -> loop start next
  in
  loop 0 0

let html_escaped_string c s = buffer_add_html_escaped_string (C.buffer c) s

let buffer_add_pct_encoded_string b s = (* Percent encoded + HTML escaped *)
  let byte = Buffer.add_char and string = Buffer.add_string in
  let unsafe_hexdig_of_int i = match i < 10 with
  | true -> Char.unsafe_chr (i + 0x30)
  | false -> Char.unsafe_chr (i + 0x37)
  in
  let flush b max start i =
    if start <= max then Buffer.add_substring b s start (i - start);
  in
  let rec loop b s max start i =
    if i > max then flush b max start i else
    let next = i + 1 in
    match String.get s i with
    | '%' (* In CommonMark destinations may have percent encoded chars *)
    (* See https://tools.ietf.org/html/rfc3986 *)
    (* unreserved *)
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~'
    (* sub-delims *)
    | '!' | '$' | (*'&' | '\'' | *) '(' | ')' | '*' | '+' | ',' | ';' | '='
    (* gen-delims *)
    | ':' | '/' | '?' | '#' | (* '[' | ']' cmark escapes them | *) '@' ->
        loop b s max start next
    | '&' -> flush b max start i; string b "&amp;"; loop b s max next next
    | '\'' -> flush b max start i; string b "&apos;"; loop b s max next next
    | c ->
        flush b max start i;
        let hi = (Char.code c lsr 4) land 0xF in
        let lo = (Char.code c) land 0xF in
        byte b '%';
        byte b (unsafe_hexdig_of_int hi);
        byte b (unsafe_hexdig_of_int lo);
        loop b s max next next
  in
  loop b s (String.length s - 1) 0 0

let pct_encoded_string c s = buffer_add_pct_encoded_string (C.buffer c) s

(* Rendering functions *)

let comment c s =
  C.string c "<!-- "; html_escaped_string c s; C.string c " -->"

let comment_undefined_label c l = match Inline.Link.referenced_label l with
| None -> () | Some def -> comment c ("Undefined label " ^ (Label.key def))

let comment_unknown_def_type c l = match Inline.Link.referenced_label l with
| None -> () | Some def ->
    comment c ("Unknown label definition type for " ^ (Label.key def))

let comment_foonote_image c l = match Inline.Link.referenced_label l with
| None -> () | Some def ->
    comment c ("Footnote " ^ (Label.key def) ^ " referenced as image")

let block_lines c = function (* newlines only between lines *)
| [] -> () | (l, _) :: ls ->
    let line c (l, _) = C.byte c '\n'; C.string c l in
    C.string c l; List.iter (line c) ls

(* Inline rendering *)

let autolink c a =
  let pre = if Inline.Autolink.is_email a then "mailto:" else "" in
  let url = pre ^ (fst (Inline.Autolink.link a)) in
  let url = if Inline.Link.is_unsafe url then "" else url in
  C.string c "<a href=\""; pct_encoded_string c url; C.string c "\">";
  html_escaped_string c (fst (Inline.Autolink.link a));
  C.string c "</a>"

let break c b = match Inline.Break.type' b with
| `Hard -> C.string c "<br>\n"
| `Soft -> C.byte c '\n'

let code_span c cs =
  C.string c "<code>";
  html_escaped_string c (Inline.Code_span.code cs);
  C.string c "</code>"

let emphasis c e =
  C.string c "<em>"; C.inline c (Inline.Emphasis.inline e); C.string c "</em>"

let strong_emphasis c e =
  C.string c "<strong>";
  C.inline c (Inline.Emphasis.inline e);
  C.string c "</strong>"

let link_dest_and_title c ld =
  let dest = match Link_definition.dest ld with
  | None -> ""
  | Some (link, _) when safe c && Inline.Link.is_unsafe link -> ""
  | Some (link, _) -> link
  in
  let title = match Link_definition.title ld with
  | None -> ""
  | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title)
  in
  dest, title

let image ?(close = " >") c i =
  match Inline.Link.reference_definition (C.get_defs c) i with
  | Some (Link_definition.Def (ld, _)) ->
      let plain_text c i =
        let lines = Inline.to_plain_text ~break_on_soft:false i in
        String.concat "\n" (List.map (String.concat "") lines)
      in
      let link, title = link_dest_and_title c ld in
      C.string c "<img src=\""; pct_encoded_string c link;
      C.string c "\" alt=\"";
      html_escaped_string c (plain_text c (Inline.Link.text i));
      C.byte c '\"';
      if title <> ""
      then (C.string c " title=\""; html_escaped_string c title; C.byte c '\"');
      C.string c close
  | Some (Block.Footnote.Def _) -> comment_foonote_image c i
  | None -> comment_undefined_label c i
  | Some _ -> comment_unknown_def_type c i

let link_footnote c l fn =
  let key = Label.key (Option.get (Inline.Link.referenced_label l)) in
  let text, label, ref = make_footnote_ref_ids c key fn in
  let is_full_ref = match Inline.Link.reference l with
  | `Ref (`Full, _, _) -> true | _ -> false
  in
  if is_full_ref then begin
    C.string c "<a href=\"#"; pct_encoded_string c label;
    C.string c "\" id=\""; html_escaped_string c ref;
    C.string c "\" role=\"doc-noteref\">";
    C.inline c (Inline.Link.text l); C.string c "</a>"
  end else begin
    C.string c "<sup><a href=\"#"; pct_encoded_string c label;
    C.string c "\" id=\""; html_escaped_string c ref;
    C.string c "\" role=\"doc-noteref\" class=\"fn-label\">";
    C.string c text; C.string c "</a></sup>"
  end

let link c l = match Inline.Link.reference_definition (C.get_defs c) l with
| Some (Link_definition.Def (ld, _)) ->
    let link, title = link_dest_and_title c ld in
    C.string c "<a href=\""; pct_encoded_string c link;
    if title <> "" then (C.string c "\" title=\""; html_escaped_string c title);
    C.string c "\">"; C.inline c (Inline.Link.text l); C.string c "</a>"
| Some (Block.Footnote.Def (fn, _)) -> link_footnote c l fn
| None -> C.inline c (Inline.Link.text l); comment_undefined_label c l
| Some _ -> C.inline c (Inline.Link.text l); comment_unknown_def_type c l

let raw_html c h =
  if safe c then comment c "CommonMark raw HTML omitted" else
  let line c (_, (h, _)) = C.byte c '\n'; C.string c h in
  if h <> []
  then (C.string c (fst (snd (List.hd h))); List.iter (line c) (List.tl h))

let strikethrough c s =
  C.string c "<del>";
  C.inline c (Inline.Strikethrough.inline s);
  C.string c "</del>"

let math_span c ms =
  let tex_line c l = html_escaped_string c (Block_line.tight_to_string l) in
  let tex_lines c = function (* newlines only between lines *)
  | [] -> () | l :: ls ->
      let line c l = C.byte c '\n'; tex_line c l in
      tex_line c l; List.iter (line c) ls
  in
  let tex = Inline.Math_span.tex_layout ms in
  if tex = [] then () else
  (C.string c (if Inline.Math_span.display ms then "\\[" else "\\(");
   tex_lines c tex;
   C.string c (if Inline.Math_span.display ms then "\\]" else "\\)"))

let inline c = function
| Inline.Autolink (a, _) -> autolink c a; true
| Inline.Break (b, _) -> break c b; true
| Inline.Code_span (cs, _) -> code_span c cs; true
| Inline.Emphasis (e, _) -> emphasis c e; true
| Inline.Image (i, _) -> image c i; true
| Inline.Inlines (is, _) -> List.iter (C.inline c) is; true
| Inline.Link (l, _) -> link c l; true
| Inline.Raw_html (html, _) -> raw_html c html; true
| Inline.Strong_emphasis (e, _) -> strong_emphasis c e; true
| Inline.Text (t, _) -> html_escaped_string c t; true
| Inline.Ext_strikethrough (s, _) -> strikethrough c s; true
| Inline.Ext_math_span (ms, _) -> math_span c ms; true
| _ -> comment c "<!-- Unknown Cmarkit inline -->"; true

(* Block rendering *)

let block_quote c bq =
  C.string c "<blockquote>\n";
  C.block c (Block.Block_quote.block bq);
  C.string c "</blockquote>\n"

let code_block c cb =
  let i = Option.map fst (Block.Code_block.info_string cb) in
  let lang = Option.bind i Block.Code_block.language_of_info_string in
  let line (l, _) = html_escaped_string c l; C.byte c '\n' in
  match lang with
  | Some (lang, _env) when backend_blocks c && lang.[0] = '=' ->
      if lang = "=html" && not (safe c)
      then block_lines c (Block.Code_block.code cb) else ()
  | _ ->
      C.string c "<pre><code";
      begin match lang with
      | None -> ()
      | Some (lang, _env) ->
          C.string c " class=\"language-"; html_escaped_string c lang;
          C.byte c '\"'
      end;
      C.byte c '>';
      List.iter line (Block.Code_block.code cb);
      C.string c "</code></pre>\n"

let heading c h =
  let level = string_of_int (Block.Heading.level h) in
  C.string c "<h"; C.string c level;
  begin match Block.Heading.id h with
  | None -> C.byte c '>';
  | Some (`Auto id | `Id id) ->
      let id = unique_id c id in
      C.string c " id=\""; C.string c id;
      C.string c "\"><a class=\"anchor\" aria-hidden=\"true\" href=\"#";
      C.string c id; C.string c "\"></a>";
  end;
  C.inline c (Block.Heading.inline h);
  C.string c "</h"; C.string c level; C.string c ">\n"

let paragraph c p =
  C.string c "<p>"; C.inline c (Block.Paragraph.inline p); C.string c "</p>\n"

let item_block ~tight c = function
| Block.Blank_line _ -> ()
| Block.Paragraph (p, _) when tight -> C.inline c (Block.Paragraph.inline p)
| Block.Blocks (bs, _) ->
    let rec loop c add_nl = function
    | Block.Blank_line _ :: bs -> loop c add_nl bs
    | Block.Paragraph (p,_) :: bs when tight ->
        C.inline c (Block.Paragraph.inline p); loop c true bs
    | b :: bs -> (if add_nl then C.byte c '\n'); C.block c b; loop c false bs
    | [] -> ()
    in
    loop c true bs
| b -> C.byte c '\n'; C.block c b

let list_item ~tight c (i, _) = match Block.List_item.ext_task_marker i with
| None ->
    C.string c "<li>";
    item_block ~tight c (Block.List_item.block i);
    C.string c "</li>\n"
| Some (mark, _) ->
    C.string c "<li>";
    let close = match Block.List_item.task_status_of_task_marker mark with
    | `Unchecked ->
        C.string c
          "<div class=\"task\"><input type=\"checkbox\" disabled><div>";
        "</div></div></li>\n"
    | `Checked | `Other _ ->
        C.string c
          "<div class=\"task\"><input type=\"checkbox\" disabled checked><div>";
        "</div></div></li>\n"
    | `Cancelled ->
        C.string c
          "<div class=\"task\"><input type=\"checkbox\" disabled><del>";
        "<del></div></li>\n"
    in
    item_block ~tight c (Block.List_item.block i);
    C.string c close

let list c l =
  let tight = Block.List'.tight l in
  match Block.List'.type' l with
  | `Unordered _ ->
      C.string c "<ul>\n";
      List.iter (list_item ~tight c) (Block.List'.items l);
      C.string c "</ul>\n"
  | `Ordered (start, _) ->
      C.string c "<ol";
      if start = 1 then C.string c ">\n" else
      (C.string c " start=\""; C.string c (string_of_int start);
       C.string c "\">\n");
      List.iter (list_item ~tight c) (Block.List'.items l);
      C.string c "</ol>\n"

let html_block c lines =
  let line (l, _) = C.string c l; C.byte c '\n' in
  if safe c then (comment c "CommonMark HTML block omitted"; C.byte c '\n') else
  List.iter line lines

let thematic_break c = C.string c "<hr>\n"

let math_block c cb =
  let line l = html_escaped_string c (Block_line.to_string l); C.byte c '\n' in
  C.string c "\\[\n";
  List.iter line (Block.Code_block.code cb);
  C.string c "\\]\n"

let table c t =
  let start c align tag =
    C.byte c '<'; C.string c tag;
    match align with
    | None -> C.byte c '>';
    | Some `Left -> C.string c " class=\"left\">"
    | Some `Center -> C.string c " class=\"center\">"
    | Some `Right -> C.string c " class=\"right\">"
  in
  let close c tag = C.string c "</"; C.string c tag; C.string c ">\n" in
  let rec cols c tag ~align count cs = match align, cs with
  | ((a, _) :: align), (col, _) :: cs ->
      start c (fst a) tag; C.inline c col; close c tag;
      cols c tag ~align (count - 1) cs
  | ((a, _) :: align), [] ->
      start c (fst a) tag; close c tag;
      cols c tag ~align (count - 1) []
  | [], (col, _) :: cs ->
      start c None tag; C.inline c col; close c tag;
      cols c tag ~align:[] (count - 1) cs
  | [], [] ->
      for i = count downto 1 do start c None tag; close c tag done;
  in
  let row c tag ~align count cs =
    C.string c "<tr>\n"; cols c tag ~align count cs; C.string c "</tr>\n";
  in
  let header c count ~align cols = row c "th" ~align count cols in
  let data c count ~align cols = row c "td" ~align count cols in
  let rec rows c col_count ~align = function
  | ((`Header cols, _), _) :: rs ->
      let align, rs = match rs with
      | ((`Sep align, _), _) :: rs -> align, rs
      | _ -> align, rs
      in
      header c col_count ~align cols; rows c col_count ~align rs
  | ((`Data cols, _), _) :: rs ->
      data c col_count ~align cols; rows c col_count ~align rs
  | ((`Sep align, _), _) :: rs -> rows c col_count ~align rs
  | [] -> ()
  in
  C.string c "<div role=\"region\"><table>\n";
  rows c (Block.Table.col_count t) ~align:[] (Block.Table.rows t);
  C.string c "</table></div>"

let block c = function
| Block.Block_quote (bq, _) -> block_quote c bq; true
| Block.Blocks (bs, _) -> List.iter (C.block c) bs; true
| Block.Code_block (cb, _) -> code_block c cb; true
| Block.Heading (h, _) -> heading c h; true
| Block.Html_block (h, _) -> html_block c h; true
| Block.List (l, _) -> list c l; true
| Block.Paragraph (p, _) -> paragraph c p; true
| Block.Thematic_break (_, _) -> thematic_break c; true
| Block.Ext_math_block (cb, _) -> math_block c cb; true
| Block.Ext_table (t, _) -> table c t; true
| Block.Blank_line _
| Block.Link_reference_definition _
| Block.Ext_footnote_definition _ -> true
| _ -> comment c "Unknown Cmarkit block"; C.byte c '\n'; true

(* XHTML rendering *)

let xhtml_block c = function
| Block.Thematic_break _ -> C.string c "<hr />\n"; true
| b -> block c b

let xhtml_inline c = function
| Inline.Break (b, _) when Inline.Break.type' b = `Hard ->
    C.string c "<br />\n"; true
| Inline.Image (i, _) ->
    image ~close:" />" c i; true
| i -> inline c i

(* Document rendering *)

let footnotes c fns =
  (* XXX we could do something about recursive footnotes and footnotes in
     footnotes here. *)
  let fns = Label.Map.fold (fun _ fn acc -> fn :: acc) fns [] in
  let fns = List.sort Stdlib.compare fns in
  let footnote c (_, id, refc, fn) =
    C.string c "<li id=\""; html_escaped_string c id; C.string c "\">\n";
    C.block c (Block.Footnote.block fn);
    C.string c "<span>";
    for r = 1 to !refc do
      C.string c "<a href=\"#"; pct_encoded_string c (footnote_ref_id id r);
      C.string c "\" role=\"doc-backlink\" class=\"fn-label\">↩︎︎";
      if !refc > 1 then
        (C.string c "<sup>"; C.string c (Int.to_string r); C.string c "</sup>");
      C.string c "</a>"
    done;
    C.string c "</span>";
    C.string c "</li>"
  in
  C.string c "<section role=\"doc-endnotes\"><ol>\n";
  List.iter (footnote c) fns;
  C.string c "</ol></section>\n"

let doc c d =
  C.block c (Doc.block d);
  let st = C.State.get c state in
  if Label.Map.is_empty st.footnotes then () else footnotes c st.footnotes;
  true

(* Renderer *)

let renderer ?backend_blocks ~safe () =
  let init_context = init_context ?backend_blocks ~safe in
  Cmarkit_renderer.make ~init_context ~inline ~block ~doc ()

let xhtml_renderer ?backend_blocks ~safe () =
  let init_context = init_context ?backend_blocks ~safe in
  let inline = xhtml_inline and block = xhtml_block in
  Cmarkit_renderer.make ~init_context ~inline ~block ~doc ()

let of_doc ?backend_blocks ~safe d =
  Cmarkit_renderer.doc_to_string (renderer ~safe ()) d

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
