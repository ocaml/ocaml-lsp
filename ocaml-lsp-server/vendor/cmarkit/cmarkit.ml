(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module String_map = Map.Make (String)
module Ascii = Cmarkit_base.Ascii
module Text = Cmarkit_base.Text
module Match = Cmarkit_base
module Textloc = Cmarkit_base.Textloc
module Meta = Cmarkit_base.Meta
module Layout = struct
  type blanks = string
  type nonrec string = string
  type nonrec char = char
  type count = int
  type indent = int
  let string ?(meta = Meta.none) s = s, meta
  let empty = string ""
end

type byte_pos = Textloc.byte_pos
type line_span = Match.line_span =
  (* Substring on a single line, hereafter abbreviated to span *)
  { line_pos : Textloc.line_pos; first : byte_pos; last : byte_pos }

type 'a node = 'a * Meta.t

module Block_line = struct
  let _list_of_string flush s = (* cuts [s] on newlines *)
    let rec loop s acc max start k =
      if k > max then List.rev (flush s start max acc) else
      if not (s.[k] = '\n' || s.[k] = '\r')
      then loop s acc max start (k + 1) else
      let acc = flush s start (k - 1) acc in
      let next = k + 1 in
      let start =
        if s.[k] = '\r' && next <= max && s.[next] = '\n' then next + 1 else
        next
      in
      loop s acc max start start
    in
    loop s [] (String.length s - 1) 0 0

  let flush ?(meta = Meta.none) s start last acc =
    let sub = String.sub s start (last - start + 1) in
    (sub, meta) :: acc

  let flush_tight ?(meta = Meta.none) s start last acc =
    (* If [s] has newlines, blanks after newlines are layout *)
    if start > last then ("", ("", meta)) :: acc else
    match acc with
    | [] (* On the first line the blanks are legit *) ->
        ("", (String.sub s start (last - start + 1), meta)) :: acc
    | acc ->
        let nb = Match.first_non_blank s ~last ~start in
        (String.sub s start (nb - 1 - start + 1),
         (String.sub s nb (last - nb + 1), meta)) :: acc

  (* Block lines *)

  type t = string node

  let to_string = fst
  let list_of_string ?meta s = _list_of_string (flush ?meta) s
  let list_textloc = function
  | [] -> Textloc.none | [(_, m)] -> Meta.textloc m
  | (_, first) :: _ as l ->
      let _, last = List.hd (List.rev l) in
      Textloc.reloc ~first:(Meta.textloc first) ~last:(Meta.textloc last)

  (* Tight lines *)

  type tight = Layout.blanks * t

  let tight_to_string l = fst (snd l)
  let tight_list_of_string ?meta s = _list_of_string (flush_tight ?meta) s
  let tight_list_textloc = function
  | [] -> Textloc.none | [_, (_, m)] -> Meta.textloc m
  | (_, (_, first)) :: _ as l ->
      let (_, (_, last)) = List.hd (List.rev l) in
      Textloc.reloc ~first:(Meta.textloc first) ~last:(Meta.textloc last)

  (* Blank lines *)

  type blank = Layout.blanks node
end

module Label = struct
  type key = string
  type t = { meta : Meta.t; key : key; text : Block_line.tight list }
  let make ?(meta = Meta.none) ~key text = { key; text; meta }
  let with_meta meta l = { l with meta }
  let meta t = t.meta
  let key t = t.key
  let text t = t.text
  let textloc t = Block_line.tight_list_textloc t.text
  let text_to_string t =
    String.concat " " (List.map Block_line.tight_to_string t.text)

  let compare l0 l1 = String.compare l0.key l1.key

  (* Definitions *)

  module Map = Map.Make (String)
  type def = ..
  type defs = def Map.t

  (* Resolvers *)

  type context =
  [ `Def of t option * t
  | `Ref of [ `Link | `Image ] * t * (t option) ]

  type resolver = context -> t option
  let default_resolver = function
  | `Def (None, k) -> Some k
  | `Def (Some _, k) -> None
  | `Ref (_, _, k) -> k
end

module Link_definition = struct
  type layout =
    { indent : Layout.indent;
      angled_dest : bool;
      before_dest : Block_line.blank list;
      after_dest : Block_line.blank list;
      title_open_delim : Layout.char;
      after_title : Block_line.blank list; }

  let layout_for_dest dest =
    let needs_angles c = Ascii.is_control c || c = ' ' in
    let angled_dest = String.exists needs_angles dest in
    { indent = 0; angled_dest; before_dest = [];
      after_dest = []; title_open_delim = '\"'; after_title = [] }

  let default_layout =
    { indent = 0; angled_dest = false; before_dest = [];
      after_dest = []; title_open_delim = '\"'; after_title = [] }

  type t =
    { layout : layout;
      label : Label.t option;
      defined_label : Label.t option;
      dest : string node option;
      title : Block_line.tight list option; }

  let make ?layout ?defined_label ?label ?dest ?title () =
    let layout = match dest with
    | None -> default_layout | Some (d, _) -> layout_for_dest d
    in
    let defined_label = match defined_label with None -> label | Some d -> d in
    { layout; label; defined_label; dest; title }

  let layout ld = ld.layout
  let label ld = ld.label
  let defined_label ld = ld.defined_label
  let dest ld = ld.dest
  let title ld = ld.title

  type Label.def += Def of t node
end

module Inline = struct
  type t = ..

  module Autolink = struct
    type t = { is_email : bool; link : string node; }
    let is_email a = a.is_email
    let link a = a.link
    let make link =
      let is_email =
        let l = String.concat "" ["<"; fst link; ">"] in
        match Match.autolink_email l ~last:(String.length l - 1) ~start:0 with
        | None -> false | Some _ -> true
      in
      { is_email; link }
  end

  module Break = struct
    type type' = [ `Hard | `Soft ]
    type t =
    { layout_before : Layout.blanks node;
      type' : type';
      layout_after : Layout.blanks node; }

    let make
        ?(layout_before = Layout.empty) ?(layout_after = Layout.empty) type'
      =
      { layout_before; type'; layout_after }

    let type' b = b.type'
    let layout_before b = b.layout_before
    let layout_after b = b.layout_after
  end

  module Code_span = struct
    type t =
      { backtick_count : Layout.count;
        code_layout : Block_line.tight list; }

    let make ~backtick_count code_layout = { backtick_count; code_layout }

    let min_backtick_count ~min counts =
      let rec loop min = function
      | c :: cs -> if min <> c then min else loop (c + 1) cs | [] -> min
      in
      loop min (List.sort Int.compare counts)

    let of_string ?(meta = Meta.none) = function
    | "" -> { backtick_count = 1 ; code_layout = ["", ("", meta)] }
    | s ->
        (* This finds out the needed backtick count, whether spaces are needed,
           and treats blanks after newline as layout *)
        let max = String.length s - 1 in
        let need_sp = s.[0] = '`' || s.[max] = '`' in
        let s = if need_sp then String.concat "" [" "; s; " "] else s in
        let backtick_counts, code_layout =
          let rec loop bt_counts acc max btc start k = match k > max with
          | true ->
              (* assert (btc = 0) because of [need_sp] *)
              bt_counts,
              if acc = [] then ["", (s, meta)] else
              List.rev (Block_line.flush_tight ~meta s start max acc)
          | false ->
              if s.[k] = '`'
              then loop bt_counts acc max (btc + 1) start (k + 1) else
              let bt_counts = if btc > 0 then btc :: bt_counts else bt_counts in
              if not (s.[k] = '\n' || s.[k] = '\r')
              then loop bt_counts acc max 0 start (k + 1) else
              let acc = Block_line.flush_tight ~meta s start (k - 1) acc in
              let start =
                if k + 1 <= max && s.[k] = '\r' && s.[k + 1] = '\n'
                then k + 2 else k + 1
              in
              loop bt_counts acc max 0 start start
          in
          loop [] [] max 0 0 0
        in
        let backtick_count = min_backtick_count ~min:1 backtick_counts in
        { backtick_count; code_layout }

    let backtick_count cs = cs.backtick_count
    let code_layout cs = cs.code_layout
    let code cs =
      (* Extract code, see https://spec.commonmark.org/0.30/#code-spans *)
      let sp c = Char.equal c ' ' in
      let s = List.map Block_line.tight_to_string cs.code_layout in
      let s = String.concat " " s in
      if s = "" then "" else
      if s.[0] = ' ' && s.[String.length s - 1] = ' ' &&
         not (String.for_all sp s)
      then String.sub s 1 (String.length s - 2) else s
  end

  module Emphasis = struct
    type inline = t
    type t = { delim : Layout.char; inline : inline }
    let make ?(delim = '*') inline = { delim; inline }
    let inline e = e.inline
    let delim e = e.delim
  end

  module Link = struct
    type inline = t

    type reference_layout = [ `Collapsed | `Full | `Shortcut ]
    type reference =
    [ `Inline of Link_definition.t node
    | `Ref of reference_layout * Label.t * Label.t ]

    type t = { text : inline; reference : reference; }

    let make text reference = { text; reference }
    let text l = l.text
    let reference l = l.reference
    let referenced_label l = match l.reference with
    | `Inline _ -> None | `Ref (_, _, k) -> Some k

    let reference_definition defs l = match l.reference with
    | `Inline ld -> Some (Link_definition.Def ld)
    | `Ref (_, _, def) -> Label.Map.find_opt (Label.key def) defs

    let is_unsafe l =
      let allowed_data_url l =
        let allowed = ["image/gif"; "image/png"; "image/jpeg"; "image/webp"] in
        (* Extract mediatype from data:[<mediatype>][;base64],<data> *)
        match String.index_from_opt l 4 ',' with
        | None -> false
        | Some j ->
            let k = match String.index_from_opt l 4 ';' with
            | None -> j | Some k -> k
            in
            let t = String.sub l 5 (min j k - 5) in
            List.mem t allowed
      in
      Ascii.caseless_starts_with ~prefix:"javascript:" l ||
      Ascii.caseless_starts_with ~prefix:"vbscript:" l ||
      Ascii.caseless_starts_with ~prefix:"file:" l ||
      (Ascii.caseless_starts_with ~prefix:"data:" l && not (allowed_data_url l))
  end

  module Raw_html = struct
    type t = Block_line.tight list
  end

  module Text = struct
    type t = string
  end

  type t +=
  | Autolink of Autolink.t node
  | Break of Break.t node
  | Code_span of Code_span.t node
  | Emphasis of Emphasis.t node
  | Image of Link.t node
  | Inlines of t list node
  | Link of Link.t node
  | Raw_html of Raw_html.t node
  | Strong_emphasis of Emphasis.t node
  | Text of Text.t node

  let empty = Inlines ([], Meta.none)

  let err_unknown = "Unknown Cmarkit.Inline.t type extension"

  (* Extensions *)

  module Strikethrough = struct
    type nonrec t = t
    let make = Fun.id
    let inline = Fun.id
  end

  module Math_span = struct
    type t = { display : bool; tex_layout : Block_line.tight list; }
    let make ~display tex_layout = { display; tex_layout }
    let display ms = ms.display
    let tex_layout ms = ms.tex_layout
    let tex ms =
      let s = List.map Block_line.tight_to_string ms.tex_layout in
      String.concat " "s
  end

  type t +=
  | Ext_strikethrough of Strikethrough.t node
  | Ext_math_span of Math_span.t node

  (* Functions on inlines *)

  let is_empty = function
  | Text ("", _) | Inlines ([], _) -> true | _ -> false

  let ext_none _ = invalid_arg err_unknown
  let meta ?(ext = ext_none) = function
  | Autolink (_, m) | Break (_, m) | Code_span (_, m) | Emphasis (_, m)
  | Image (_, m) | Inlines (_, m) | Link (_, m) | Raw_html (_, m)
  | Strong_emphasis (_, m)  | Text (_, m) -> m
  | Ext_strikethrough (_, m) -> m | Ext_math_span (_, m) -> m
  | i -> ext i

  let rec normalize ?(ext = ext_none) = function
  | Autolink _ | Break _ | Code_span _ | Raw_html _ | Text _
  | Inlines ([], _) | Ext_math_span _ as i -> i
  | Image (l, m) -> Image ({ l with text = normalize ~ext l.text }, m)
  | Link (l, m) -> Link ({ l with text = normalize ~ext l.text }, m)
  | Inlines ([i], _) -> i
  | Emphasis (e, m) ->
      Emphasis ({ e with inline = normalize ~ext e.inline}, m)
  | Strong_emphasis (e, m) ->
      Strong_emphasis ({ e with inline = normalize ~ext e.inline}, m)
  | Inlines (i :: is, m) ->
      let rec loop acc = function
      | Inlines (is', m) :: is -> loop acc (List.rev_append (List.rev is') is)
      | Text (t', m') as i' :: is ->
          begin match acc with
          | Text (t, m) :: acc ->
              let tl = Textloc.span (Meta.textloc m) (Meta.textloc m') in
              let i = Text (t ^ t', Meta.with_textloc ~keep_id:true m tl) in
              loop (i :: acc) is
          | _ -> loop (normalize ~ext i' :: acc) is
          end
      | i :: is -> loop (normalize ~ext i :: acc) is
      | [] -> List.rev acc
      in
      let is = loop [normalize ~ext i] is in
      (match is with [i] -> i | _ -> Inlines (is, m))
  | Ext_strikethrough (i, m) -> Ext_strikethrough (normalize ~ext i, m)
  | i -> ext i

  let ext_none ~break_on_soft = ext_none
  let to_plain_text ?(ext = ext_none) ~break_on_soft i =
    let push s acc = (s :: List.hd acc) :: List.tl acc in
    let newline acc = [] :: (List.rev (List.hd acc)) :: List.tl acc in
    let rec loop ~break_on_soft acc = function
    | Autolink (a, _) :: is ->
        let acc = push (String.concat "" ["<"; fst a.link; ">"]) acc in
        loop ~break_on_soft acc is
    | Break ({ type' = `Hard }, _) :: is ->
        loop ~break_on_soft (newline acc) is
    | Break ({ type' = `Soft }, _) :: is ->
        let acc = if break_on_soft then newline acc else (push " " acc) in
        loop ~break_on_soft acc is
    | Code_span (cs, _) :: is ->
        loop ~break_on_soft (push (Code_span.code cs) acc) is
    | Emphasis ({ inline }, _) :: is | Strong_emphasis ({ inline }, _) :: is ->
        loop ~break_on_soft acc (inline :: is)
    | Inlines (is', _) :: is ->
        loop ~break_on_soft acc (List.rev_append (List.rev is') is)
    | Link (l, _) :: is | Image (l, _) :: is ->
        loop ~break_on_soft acc (l.text :: is)
    | Raw_html _ :: is ->
        loop ~break_on_soft acc is
    | Text (t, _) :: is ->
        loop ~break_on_soft (push t acc) is
    | Ext_strikethrough (i, _) :: is ->
        loop ~break_on_soft acc (i :: is)
    | Ext_math_span (m, _) :: is ->
        loop ~break_on_soft (push (Math_span.tex m) acc) is
    | i :: is ->
        loop ~break_on_soft acc (ext ~break_on_soft i :: is)
    | [] ->
        List.rev ((List.rev (List.hd acc)) :: List.tl acc)
    in
    loop ~break_on_soft ([] :: []) [i]

  let id ?buf ?ext i =
    let text = to_plain_text ?ext ~break_on_soft:false i in
    let s = String.concat "\n" (List.map (String.concat "") text) in
    let b = match buf with
    | Some b -> Buffer.reset b; b | None -> Buffer.create 256
    in
    let[@inline] collapse_blanks b ~prev_byte =
      (* Collapses non initial white *)
      if Ascii.is_blank prev_byte && Buffer.length b <> 0
      then Buffer.add_char b '-'
    in
    let rec loop b s max ~prev_byte k =
      if k > max then Buffer.contents b else
      match s.[k] with
      | ' ' | '\t' as prev_byte -> loop b s max ~prev_byte (k + 1)
      | '_' | '-' as c ->
          collapse_blanks b ~prev_byte;
          Buffer.add_char b c;
          loop b s max ~prev_byte:c (k + 1)
      | c ->
          let () = collapse_blanks b ~prev_byte in
          let d = String.get_utf_8_uchar s k in
          let u = Uchar.utf_decode_uchar d in
          let u = match Uchar.to_int u with 0x0000 -> Uchar.rep | _ -> u in
          let k' = k + Uchar.utf_decode_length d in
          if Cmarkit_data.is_unicode_punctuation u
          then loop b s max ~prev_byte:'\x00' k' else
          let () = match Cmarkit_data.unicode_case_fold u with
          | None -> Buffer.add_utf_8_uchar b u
          | Some fold -> Buffer.add_string b fold
          in
          let prev_byte = s.[k] in
          loop b s max ~prev_byte k'
    in
    loop b s (String.length s - 1) ~prev_byte:'\x00' 0
end

(* Blocks *)

module Block = struct
  type t = ..

  module Blank_line = struct
    type t = Layout.blanks
  end

  module Block_quote = struct
    type nonrec t = { indent : Layout.indent; block : t; }
    let make ?(indent = 0) block = { indent; block }
    let indent bq = bq.indent
    let block bq = bq.block
  end

  module Code_block = struct
    type fenced_layout =
      { indent : Layout.indent;
        opening_fence : Layout.string node;
        closing_fence : Layout.string node option; }

    let default_fenced_layout =
      { indent = 0;
        opening_fence = Layout.empty;
        closing_fence = Some Layout.empty }

    type layout = [ `Indented | `Fenced of fenced_layout ]
    type t =
      { layout : layout;
        info_string : string node option;
        code : string node list; }

    let make ?(layout = `Fenced default_fenced_layout) ?info_string code =
      let layout = match info_string, layout with
      | Some _, `Indented -> `Fenced default_fenced_layout
      | _, layout -> layout
      in
      { layout; info_string; code }

    let layout cb = cb.layout
    let info_string cb = cb.info_string
    let code cb = cb.code

    let make_fence cb =
      let rec loop char counts = function
      | [] -> counts
      | (c, _) :: cs ->
          let max = String.length c - 1 in
          let k = ref 0 in
          while (!k <= max && c.[!k] = char) do incr k done;
          loop char (if !k <> 0 then !k :: counts else counts) cs
      in
      let char = match cb.info_string with
      | Some (i, _) when String.exists (Char.equal '`') i -> '~'
      | None | Some _ -> '`'
      in
      let counts = loop char [] cb.code in
      char,
      Inline.Code_span.min_backtick_count (* not char specific *) ~min:3 counts

    let language_of_info_string s =
      let rec next_white s max i =
        if i > max || Ascii.is_white s.[i] then i else
        next_white s max (i + 1)
      in
      if s = "" then None else
      let max = String.length s - 1 in
      let white = next_white s max 0 in
      let rem_first = Match.first_non_blank s ~last:max ~start:white in
      let lang = String.sub s 0 white in
      if lang = "" then None else
      Some (lang, String.sub s rem_first (max - rem_first + 1))

    let is_math_block = function
    | None -> false | Some (i, _) -> match language_of_info_string i with
    | Some ("math", _) -> true
    | Some _ | None -> false
  end

  module Heading = struct
    type atx_layout =
      { indent : Layout.indent;
        after_opening : Layout.blanks;
        closing : Layout.string; }

    let default_atx_layout = { indent = 0; after_opening = ""; closing = "" }

    type setext_layout =
      { leading_indent : Layout.indent;
        trailing_blanks : Layout.blanks;
        underline_indent : Layout.indent;
        underline_count : Layout.count node;
        underline_blanks : Layout.blanks; }

    type layout = [ `Atx of atx_layout | `Setext of setext_layout ]
    type id = [ `Auto of string | `Id of string ]
    type t = { layout : layout; level : int; inline : Inline.t; id : id option }

    let make ?id ?(layout = `Atx default_atx_layout) ~level inline =
      let max = match layout with `Atx _ -> 6 | `Setext _ -> 2 in
      let level = Int.max 1 (Int.min level max) in
      {layout; level; inline; id}

    let layout h = h.layout
    let level h = h.level
    let inline h = h.inline
    let id h = h.id
  end

  module Html_block = struct
    type t = string node list
  end

  module List_item = struct
    type block = t
    type t =
      { before_marker : Layout.indent;
        marker : Layout.string node;
        after_marker : Layout.indent;
        block : block;
        ext_task_marker : Uchar.t node option }

    let make
        ?(before_marker = 0) ?(marker = Layout.empty) ?(after_marker = 1)
        ?ext_task_marker block
      =
      { before_marker; marker; after_marker; block; ext_task_marker }

    let block i = i.block
    let before_marker i = i.before_marker
    let marker i = i.marker
    let after_marker i = i.after_marker
    let ext_task_marker i = i.ext_task_marker
    let task_status_of_task_marker u = match Uchar.to_int u with
    | 0x0020 -> `Unchecked
    | 0x0078 (* x *) | 0x0058 (* X *) | 0x2713 (* ✓ *) | 0x2714 (* ✔ *)
    | 0x10102 (* 𐄂 *) | 0x1F5F8 (* 🗸*) -> `Checked
    | 0x007E (* ~ *) -> `Cancelled
    | _ -> `Other u
  end

  module List' = struct
    type type' = [ `Unordered of Layout.char | `Ordered of int * Layout.char ]
    type t =
      { type' : type';
        tight : bool;
        items : List_item.t node list; }

    let make ?(tight = true) type' items = { type'; tight; items }

    let type' l = l.type'
    let tight l = l.tight
    let items l = l.items
  end

  module Paragraph = struct
    type t =
      { leading_indent : Layout.indent;
        inline : Inline.t;
        trailing_blanks : Layout.blanks; }

    let make ?(leading_indent = 0) ?(trailing_blanks = "") inline =
      { leading_indent; inline; trailing_blanks }

    let inline p = p.inline
    let leading_indent p = p.leading_indent
    let trailing_blanks p = p.trailing_blanks
  end

  module Thematic_break = struct
    type t = { indent : Layout.indent; layout : Layout.string }
    let make ?(indent = 0) ?(layout = "---") () =  { indent; layout }
    let indent t = t.indent
    let layout t = t.layout
  end

  type t +=
  | Blank_line of Layout.blanks node
  | Block_quote of Block_quote.t node
  | Blocks of t list node
  | Code_block of Code_block.t node
  | Heading of Heading.t node
  | Html_block of Html_block.t node
  | Link_reference_definition of Link_definition.t node
  | List of List'.t node
  | Paragraph of Paragraph.t node
  | Thematic_break of Thematic_break.t node

  let empty = Blocks ([], Meta.none)

  (* Extensions *)

  module Table = struct
    type align = [ `Left | `Center | `Right ]
    type sep = align option * Layout.count
    type cell_layout = Layout.blanks * Layout.blanks
    type row =
    [ `Header of (Inline.t * cell_layout) list
    | `Sep of sep node list
    | `Data of (Inline.t * cell_layout) list ]

    type t =
      { indent : Layout.indent;
        col_count : int;
        rows : (row node * Layout.blanks) list }

    let col_count rows =
      let rec loop c = function
      | (((`Header cols | `Data cols), _), _) :: rs ->
          loop (Int.max (List.length cols) c) rs
      | (((`Sep cols), _), _) :: rs ->
          loop (Int.max (List.length cols) c) rs
      | [] -> c
      in
      loop 0 rows

    let make ?(indent = 0) rows = { indent; col_count = col_count rows; rows }
    let indent t = t.indent
    let col_count t = t.col_count
    let rows t = t.rows

    let parse_sep_row cs =
      let rec loop acc = function
      | [] -> Some (List.rev acc)
      | (Inline.Text (s, meta), ("", "")) :: cs ->
          if s = "" then None else
          let max = String.length s - 1 in
          let first_colon = s.[0] = ':' and  last_colon = s.[max] = ':' in
          let first = if first_colon then 1 else 0 in
          let last = if last_colon then max - 1 else max in
          begin
            match
              for i = first to last do if s.[i] <> '-' then raise Exit; done
            with
            | exception Exit -> None
            | () ->
                let count = last - first + 1 in
                let sep = match first_colon, last_colon with
                | false, false -> None
                | true, true -> Some `Center
                | true, false -> Some `Left
                | false, true -> Some `Right
                in
                loop (((sep, count), meta) :: acc) cs
          end
      | _ -> None
      in
      loop [] cs
  end

  module Footnote = struct
    type nonrec t =
      { indent : Layout.indent;
        label : Label.t;
        defined_label : Label.t option;
        block : t }

    let make ?(indent = 0) ?defined_label:d label block =
      let defined_label = match d with None -> Some label | Some d -> d in
      { indent; label; defined_label; block }

    let indent fn = fn.indent
    let label fn = fn.label
    let defined_label fn = fn.defined_label
    let block fn = fn.block

    type Label.def += Def of t node
    let stub label defined_label =
      Def ({ indent = 0; label; defined_label; block = empty}, Meta.none)
  end

  type t +=
  | Ext_math_block of Code_block.t node
  | Ext_table of Table.t node
  | Ext_footnote_definition of Footnote.t node

  (* Functions on blocks *)

  let err_unknown = "Unknown Cmarkit.Block.t type extension"

  let ext_none _ = invalid_arg err_unknown
  let meta ?(ext = ext_none) = function
  | Blank_line (_, m) | Block_quote (_, m) | Blocks (_, m) | Code_block (_, m)
  | Heading (_, m) | Html_block (_, m) | Link_reference_definition (_, m)
  | List (_, m) | Paragraph (_, m) | Thematic_break (_, m)
  | Ext_math_block (_, m) | Ext_table (_, m)
  | Ext_footnote_definition (_, m) -> m
  | b -> ext b

  let rec normalize ?(ext = ext_none) = function
  | Blank_line _ | Code_block _ | Heading _ | Html_block _
  | Link_reference_definition _ | Paragraph _ | Thematic_break _
  | Blocks ([], _) | Ext_math_block _ | Ext_table _ as b -> b
  | Block_quote (b, m) ->
      let b = { b with block = normalize ~ext b.block } in
      Block_quote (b, m)
  | List (l, m) ->
      let item (i, meta) =
        let block = List_item.block i in
        { i with List_item.block = normalize ~ext block }, meta
      in
      List ({ l with items = List.map item l.items }, m)
  | Blocks (b :: bs, m) ->
      let rec loop acc = function
      | Blocks (bs', m) :: bs -> loop acc (List.rev_append (List.rev bs') bs)
      | b :: bs -> loop (normalize ~ext b :: acc) bs
      | [] -> List.rev acc
      in
      let bs = loop [normalize ~ext b] bs in
      (match bs with [b] -> b | _ -> Blocks (bs, m))
  | Ext_footnote_definition (fn, m) ->
      let fn = { fn with block = normalize ~ext fn.block } in
      Ext_footnote_definition (fn, m)
  | b -> ext b

  let rec defs
      ?(ext = fun b defs -> invalid_arg err_unknown) ?(init = Label.Map.empty)
    = function
    | Blank_line _ | Code_block _ | Heading _ | Html_block _
    | Paragraph _ | Thematic_break _
    | Ext_math_block _ | Ext_table _ -> init
    | Block_quote (b, _) -> defs ~ext ~init (Block_quote.block b)
    | Blocks (bs, _) -> List.fold_left (fun init b -> defs ~ext ~init b) init bs
    | List (l, _) ->
        let add init (i, _) = defs ~ext ~init (List_item.block i) in
        List.fold_left add init l.items
    | Link_reference_definition ld ->
        begin match Link_definition.defined_label (fst ld) with
        | None -> init
        | Some def ->
            Label.Map.add (Label.key def) (Link_definition.Def ld) init
        end
    | Ext_footnote_definition fn ->
        let init = match Footnote.defined_label (fst fn) with
        | None -> init
        | Some def -> Label.Map.add (Label.key def) (Footnote.Def fn) init
        in
        defs ~ext ~init (Footnote.block (fst fn))
    | b -> ext init b
end

(* Parsing *)

(* Closer indexes.

   They map closing delimiters to the position where they
   start. Shortcuts forward searches in inline parsing. See
   Inline_struct. *)

module Pos_set = Set.Make (Int) (* Sets of positions. *)
module Closer = struct
  type t =
  | Backticks of int (* run length *)
  | Right_brack
  | Right_paren (* Only for ruling out pathological cases. *)
  | Emphasis_marks of char
  | Strikethrough_marks
  | Math_span_marks of int (* run length *)

  let compare = Stdlib.compare
end

module Closer_index = struct
  include Map.Make (Closer)
  type nonrec t = Pos_set.t t

  let add cl pos cidx =
    let add = function
    | None -> Some (Pos_set.singleton pos)
    | Some occs -> Some (Pos_set.add pos occs)
    in
    update cl add cidx

  let closer_pos cl ~after cidx = match find_opt cl cidx with
  | None -> None
  | Some occs -> Pos_set.find_first_opt (fun pos -> pos > after) occs

  let closer_exists cl ~after cidx = match closer_pos cl ~after cidx with
  | None -> false | Some _ -> true
end

(* Columns. That notion is needed to handle tab stops.
   See https://spec.commonmark.org/current/#tabs *)

type col = int
let[@inline] next_tab_stop col = (col + 4) land (lnot 3)

(* Parser abstraction *)

type parser =
  { file : Textloc.fpath (* input file name *);
    i : string (* input string *);
    buf : Buffer.t (* scratch buffer. *);
    exts : bool; (* parse extensions if [true]. *)
    nolocs : bool; (* do not compute locations if [true]. *)
    nolayout : bool; (* do not compute layout fields if [true]. *)
    heading_auto_ids : bool; (* compute heading ids. *)
    nested_links : bool;
    mutable defs : Label.defs;
    resolver : Label.resolver;
    mutable cidx : Closer_index.t; (* For inline parsing. *)
    (* Current line (only used during block parsing) *)
    mutable current_line_pos : Textloc.line_pos;
    mutable current_line_last_char :
      (* first char of line - 1 on empty lines *) Textloc.byte_pos;
    mutable current_char : Textloc.byte_pos;
    mutable current_char_col : col;
    mutable next_non_blank :
      (* current_line_last_char + 1 if none. *) Textloc.byte_pos;
    mutable next_non_blank_col : col;
    mutable tab_consumed_cols :
      (* number of cols consumed from the tab if i.[current_char] is '\t' *)
      col; }

let parser
    ?(defs = Label.Map.empty) ?(resolver = Label.default_resolver)
    ?(nested_links = false) ?(heading_auto_ids = false) ?(layout = false)
    ?(locs = false) ?(file = Textloc.file_none) ~strict i
  =
  let nolocs = not locs and nolayout = not layout and exts = not strict in
  { file; i; buf = Buffer.create 512; exts; nolocs; nolayout;
    heading_auto_ids; nested_links; defs; resolver; cidx = Closer_index.empty;
    current_line_pos = 1, 0; current_line_last_char = -1; current_char = 0;
    current_char_col = 0; next_non_blank = 0; next_non_blank_col = 0;
    tab_consumed_cols = 0; }

let find_label_defining_key p key = match Label.Map.find_opt key p.defs with
| Some (Link_definition.Def ld) -> Link_definition.defined_label (fst ld)
| Some (Block.Footnote.Def fn) -> Block.Footnote.defined_label (fst fn)
| None -> None
| _ -> assert false

let set_label_def p l def = p.defs <- Label.Map.add (Label.key l) def p.defs
let def_label p l =
  p.resolver (`Def (find_label_defining_key p (Label.key l), l))

let find_def_for_ref ~image p ref =
  let kind = if image then `Image else `Link in
  let def = find_label_defining_key p (Label.key ref) in
  p.resolver (`Ref (kind, ref, def))

let debug_span p s = String.sub p.i s.first (s.last - s.first + 1)
let debug_line p =
  let first = snd p.current_line_pos and last = p.current_line_last_char in
  String.sub p.i first (last - first + 1)

let current_line_span p ~first ~last =
  { line_pos = p.current_line_pos; first; last }

(* Making metas and text locations. This is centralized here to be able
   to disable their creation which has a non-negligible impact on
   performance. *)

let meta p textloc = if p.nolocs then Meta.none else Meta.make ~textloc ()

let textloc_of_span p span =
  if p.nolocs then Textloc.none else
  let first_byte = span.first and last_byte = span.last in
  let first_line = span.line_pos and last_line = span.line_pos in
  Textloc.v ~file:p.file ~first_byte ~last_byte ~first_line ~last_line

let textloc_of_lines p ~first ~last ~first_line ~last_line =
  if p.nolocs then Textloc.none else
  let first_byte = first and first_line = first_line.line_pos in
  let last_byte = last and last_line = last_line.line_pos in
  Textloc.v ~file:p.file ~first_byte ~last_byte ~first_line ~last_line

let meta_of_spans p ~first:first_line ~last:last_line =
  if p.nolocs then Meta.none else
  let first = first_line.first and last = last_line.last in
  meta p (textloc_of_lines p ~first ~last ~first_line ~last_line)

let meta_of_metas p ~first ~last =
  if p.nolocs then Meta.none else
  meta p (Textloc.span (Meta.textloc first) (Meta.textloc last))

let clean_raw_span ?pad p span =
  Text.utf_8_clean_raw ?pad p.buf p.i ~first:span.first ~last:span.last,
  meta p (textloc_of_span p span)

let clean_unref_span p span =
  Text.utf_8_clean_unref p.buf p.i ~first:span.first ~last:span.last,
  meta p (textloc_of_span p span)

let clean_unesc_unref_span p span =
  Text.utf_8_clean_unesc_unref p.buf p.i ~first:span.first ~last:span.last,
  meta p (textloc_of_span p span)

let layout_clean_raw_span ?pad p span =
  if p.nolayout then Layout.empty else clean_raw_span ?pad p span

let layout_clean_raw_span' ?pad p span =
  (* Like [layout_raw_span] but no meta *)
  if p.nolayout then "" else
  Text.utf_8_clean_raw ?pad p.buf p.i ~first:span.first ~last:span.last

let _tight_block_lines xxx_span p ~rev_spans =
  let rec loop p acc = function
  | [] -> acc
  | [_, fst_line] -> ("", xxx_span p fst_line) :: acc
  | (line_start, span) :: spans ->
      let acc =
        let layout =
          if p.nolayout || span.first <= line_start then "" else
          Text.utf_8_clean_raw p.buf p.i ~first:line_start
            ~last:(span.first - 1)
        in
        (layout, xxx_span p span) :: acc
      in
      loop p acc spans
  in
  loop p [] rev_spans

let tight_block_lines p ~rev_spans =
  _tight_block_lines clean_unesc_unref_span p ~rev_spans

let raw_tight_block_lines p ~rev_spans =
  _tight_block_lines clean_raw_span p ~rev_spans

let first_non_blank_in_span p s = Match.first_non_blank_in_span p.i s
let first_non_blank_over_nl ~next_line p lines line ~start =
  match Match.first_non_blank_over_nl ~next_line p.i lines ~line ~start with
  | `None -> None
  | `This_line non_blank ->
      let layout =
        if non_blank = start then [] else
        [clean_raw_span p { line with first = start ; last = non_blank - 1}]
      in
      Some (lines, line, layout, non_blank)
  | `Next_line (lines, newline, non_blank) ->
      let first_layout = clean_raw_span p { line with first = start } in
      let next_layout = clean_raw_span p { newline with last = non_blank -1 } in
      let layout = [first_layout; next_layout] in
      Some (lines, newline, layout, non_blank)

(* Inline structure parsing *)

module Inline_struct = struct

  (* Tokens for parsing inlines.

     The list of tokens of a paragraph are the points to consider to
     parse it into inlines. Tokens gradually become [Inline] tokens
     containing parsed inlines. Between two tokens there is implicit
     textual data. This data gradually becomes part of [Inline] tokens
     or, at the end of of the parsing process, becomes [Text] inlines.

     The token list also represents newlines explicitly, either via
     the [Newline] token or via the [Inline] token since inlines may
     start on a line and up on another one. *)

  type emphasis_marks =
    { start : byte_pos;
      char : char;
      count : int;
      may_open : bool;
      may_close : bool }

  type strikethrough_marks =
    { start : byte_pos;
      may_open : bool;
      may_close : bool }

  type math_span_marks =
    { start : byte_pos;
      count : int;
      may_open : bool;
      may_close : bool; }

  type token =
  | Autolink_or_html_start of { start : byte_pos }
  | Backticks of
      { start : byte_pos;
        count : int;
        escaped : bool }
  | Emphasis_marks of emphasis_marks
  | Inline of
      { start : byte_pos;
        inline : Inline.t;
        endline : line_span;
        next : byte_pos }
  | Link_start of
      { start : byte_pos;
        image : bool }
  | Newline of
      { start : (* points on spaces or \ on the broken line *) byte_pos;
        break_type : Inline.Break.type';
        newline : line_span; }
  | Right_brack of { start : byte_pos }
  | Right_paren of { start : byte_pos } (* Only used for closer index *)
  | Strikethrough_marks of strikethrough_marks
  | Math_span_marks of math_span_marks

  let token_start = function
  | Autolink_or_html_start { start } | Backticks { start }
  | Emphasis_marks { start } | Inline { start } -> start |  Link_start { start }
  | Newline { start } | Right_brack { start } -> start
  | Right_paren { start } -> start
  | Strikethrough_marks { start } -> start
  | Math_span_marks { start } -> start

  let has_backticks ~count ~after cidx =
    Closer_index.closer_exists (Closer.Backticks count) ~after cidx

  let has_right_brack ~after cidx =
    Closer_index.closer_exists Closer.Right_brack ~after cidx

  let has_right_paren ~after cidx =
    Closer_index.closer_exists Closer.Right_paren ~after cidx

  let emphasis_closer_pos ~char ~after cidx =
    Closer_index.closer_pos (Closer.Emphasis_marks char) ~after cidx

  let has_emphasis_closer ~char ~after cidx =
    Closer_index.closer_exists (Closer.Emphasis_marks char) ~after cidx

  let has_strikethrough_closer ~after cidx =
    Closer_index.closer_exists Closer.Strikethrough_marks ~after cidx

  let has_math_span_closer ~count ~after cidx =
    Closer_index.closer_exists (Closer.Math_span_marks count) ~after cidx

  let rev_token_list_and_make_closer_index toks =
    let rec loop cidx acc = function
    | Backticks { start; count; _ } as t :: toks ->
        let cidx = Closer_index.add (Closer.Backticks count) start cidx in
        loop cidx (t :: acc) toks
    | Right_brack { start } as t :: toks ->
        let cidx = Closer_index.add Closer.Right_brack start cidx in
        loop cidx (t :: acc) toks
    | Right_paren { start } :: toks ->
        let cidx = Closer_index.add Closer.Right_paren start cidx in
        loop cidx (* we don't use the token for parsing *) acc toks
    | Emphasis_marks { start; char; may_close = true } as t :: toks ->
        let cidx = Closer_index.add (Closer.Emphasis_marks char) start cidx in
        loop cidx (t :: acc) toks
    | Strikethrough_marks { start; may_close = true } as t :: toks ->
        let cidx = Closer_index.add Closer.Strikethrough_marks start cidx in
        loop cidx (t :: acc) toks
    | Math_span_marks { start; count; may_close = true } as t :: toks ->
        let cidx = Closer_index.add (Closer.Math_span_marks count) start cidx in
        loop cidx (t :: acc) toks
    | t :: toks -> loop cidx (t :: acc) toks
    | [] -> cidx, acc
    in
    loop Closer_index.empty [] toks

  let rec rev_tokens_and_shorten_last_line ~to_last:last acc = function
  (* Used to make the text delimitation precise for nested inlines *)
  | Newline ({ newline; _  } as nl) :: toks ->
      let t = Newline { nl with newline = { newline with last }} in
      List.rev_append toks (t :: acc)
  | Inline ({ endline; _ } as i) :: toks ->
      let t = Inline { i with endline = { endline with last }} in
      List.rev_append toks (t :: acc)
  | t :: toks -> rev_tokens_and_shorten_last_line ~to_last:last (t :: acc) toks
  | [] -> acc

  let rec drop_stop_after_right_brack = function
  | Right_brack _ :: toks -> toks
  | _ :: toks -> drop_stop_after_right_brack toks
  | [] -> []

  let rec drop_until ~start = function
  | t :: toks when token_start t < start -> drop_until ~start toks
  | toks -> toks

  let rec next_line = function
  (* N.B. when we use this function considering Inline tokens is not needed. *)
  | [] -> None
  | Newline { newline; _ } :: toks -> Some (toks, newline)
  | _ :: toks -> next_line toks

  (* Tokenization *)

  let newline_token s prev_line newline =
    (* https://spec.commonmark.org/current/#softbreak *)
    (* https://spec.commonmark.org/current/#hard-line-breaks *)
    let start (* includes spaces or '\\' on prev line *), break_type =
      let first = prev_line.first and last = prev_line.last in
      let non_space = Match.rev_drop_spaces s ~first ~start:last in
      if non_space = last && s.[non_space] = '\\' then (non_space, `Hard) else
      let start = non_space + 1 in
      (start, if last - start + 1 >= 2 then `Hard else `Soft)
    in
    Newline { start; break_type; newline }

  let add_backtick_token acc s line ~prev_bslash ~start =
    let last = Match.run_of ~char:'`' s ~last:line.last ~start:(start + 1) in
    let count = last - start + 1 and escaped = prev_bslash in
    Backticks {start; count; escaped} :: acc, last + 1

  let try_add_image_link_start_token acc s line ~start =
    let next = start + 1 in
    if next > line.last || s.[next] <> '[' then acc, next else
    Link_start { start; image = true } :: acc, next + 1

  let try_add_emphasis_token acc s line ~start =
    let first = line.first and last = line.last and char = s.[start] in
    let run_last = Match.run_of ~char ~last s ~start:(start + 1) in
    let count = run_last - start + 1 in
    let prev_uchar = Match.prev_uchar s ~first ~before:start in
    let next_uchar = Match.next_uchar s ~last ~after:run_last in
    let prev_white = Cmarkit_data.is_unicode_whitespace prev_uchar in
    let next_white = Cmarkit_data.is_unicode_whitespace next_uchar in
    let prev_punct = Cmarkit_data.is_unicode_punctuation prev_uchar in
    let next_punct = Cmarkit_data.is_unicode_punctuation next_uchar in
    let is_left_flanking =
      not next_white && (not next_punct || (prev_white || prev_punct))
    in
    let is_right_flanking =
      not prev_white && (not prev_punct || (next_white || next_punct))
    in
    let next = run_last + 1 in
    if not is_left_flanking && not is_right_flanking then acc, next else
    let may_open =
      (char = '*' && is_left_flanking) ||
      (char = '_' && is_left_flanking && (not is_right_flanking || prev_punct))
    in
    let may_close =
      (char = '*' && is_right_flanking) ||
      (char = '_' && is_right_flanking && (not is_left_flanking || next_punct))
    in
    if not may_open && not may_close then acc, next else
    Emphasis_marks { start; char; count; may_open; may_close } :: acc, next

  let try_add_strikethrough_marks_token acc s line ~start =
    let first = line.first and last = line.last and char = s.[start] in
    let run_last = Match.run_of ~char ~last s ~start:(start + 1) in
    let count = run_last - start + 1 in
    let next = run_last + 1 in
    if count <> 2 then acc, next else
    let prev_uchar = Match.prev_uchar s ~first ~before:start in
    let next_uchar = Match.next_uchar s ~last ~after:run_last in
    let may_close = not (Cmarkit_data.is_unicode_whitespace prev_uchar) in
    let may_open = not (Cmarkit_data.is_unicode_whitespace next_uchar) in
    if not may_open && not may_close then acc, next else
    Strikethrough_marks { start; may_open; may_close } :: acc, next

  let try_add_math_span_marks_token acc s line ~start =
    let first = line.first and last = line.last and char = s.[start] in
    let run_last = Match.run_of ~char ~last s ~start:(start + 1) in
    let count = run_last - start + 1 in
    let next = run_last + 1 in
    if count > 2 then acc, next else
    let may_open, may_close =
      if count <> 1 then true, true else
      let prev_uchar = Match.prev_uchar s ~first ~before:start in
      let next_uchar = Match.next_uchar s ~last ~after:run_last in
      let may_close = not (Cmarkit_data.is_unicode_whitespace prev_uchar) in
      let may_open = not (Cmarkit_data.is_unicode_whitespace next_uchar) in
      may_open, may_close
    in
    if not may_open && not may_close then acc, next else
    Math_span_marks { start; count; may_open; may_close } :: acc, next

  let tokenize ~exts s lines =
    (* For inlines this is where we conditionalize for extensions. All code
       paths after that no longer check for p.exts: there just won't be
       extension data to process if [exts] was not [true] here. *)
    let rec loop ~exts s lines line ~prev_bslash acc k =
      if k > line.last then match lines with
      | [] -> rev_token_list_and_make_closer_index acc
      | newline :: lines ->
          let t = newline_token s line newline in
          loop ~exts s lines newline ~prev_bslash:false (t :: acc) newline.first
      else
      if s.[k] = '\\'
      then loop ~exts s lines line ~prev_bslash:(not prev_bslash) acc (k+1) else
      let acc, next = match s.[k] with
      | '`' -> add_backtick_token acc s line ~prev_bslash ~start:k
      | c when prev_bslash -> acc, k + 1
      | '*' | '_' -> try_add_emphasis_token acc s line ~start:k
      | ']' -> Right_brack { start = k } :: acc, k + 1
      | '[' -> Link_start { start = k; image = false } :: acc, k + 1
      | '!' -> try_add_image_link_start_token acc s line ~start:k
      | '<' -> Autolink_or_html_start { start = k } :: acc, k + 1
      | ')' -> Right_paren { start = k } :: acc, k + 1
      | '~' when exts -> try_add_strikethrough_marks_token acc s line ~start:k
      | '$' when exts -> try_add_math_span_marks_token acc s line ~start:k
      | _ -> acc, k + 1
      in
      loop ~exts s lines line ~prev_bslash:false acc next
    in
    let line = List.hd lines and lines = List.tl lines in
    let cidx, toks = loop ~exts s lines line ~prev_bslash:false [] line.first in
    cidx, toks, line

  (* Making inlines and inline tokens *)

  let break_inline p line ~start ~break_type:type' ~newline =
    let layout_before = { line with first = start } in
    let layout_after =
      let non_blank = first_non_blank_in_span p newline in
      { newline with last = non_blank - 1 }
    in
    let m = meta_of_spans p ~first:layout_before ~last:layout_after in
    let layout_before = layout_clean_raw_span p layout_before in
    let layout_after = layout_clean_raw_span p layout_after in
    Inline.Break ({ layout_before; type'; layout_after }, m)

  let try_add_text_inline p line ~first ~last acc =
    if first > last then acc else
    let first = match first = line.first with
    | true -> first_non_blank_in_span p line (* strip leading blanks *)
    | false -> first
    in
    Inline.Text (clean_unesc_unref_span p { line with first; last }) :: acc

  let inlines_inline p ~first ~last ~first_line ~last_line = function
  | [i] -> i
  | is ->
      let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
      Inline.Inlines (is, meta p textloc)

  let code_span_token p ~count ~first ~last ~first_line ~last_line rev_spans =
    let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
    let code_layout = raw_tight_block_lines p ~rev_spans in
    let meta = meta p textloc in
    let cs = Inline.Code_span ({ backtick_count = count; code_layout }, meta) in
    Inline { start = first; inline = cs; endline = last_line; next = last + 1 }

  let autolink_token p line ~first ~last ~is_email =
    let meta = meta p (textloc_of_span p { line with first; last }) in
    let link = { line with first = first + 1; last = last - 1 } in
    let link = clean_unref_span p link in
    let inline = Inline.Autolink ({ is_email; link }, meta) in
    Inline { start = first; inline; endline = line; next = last + 1 }

  let raw_html_token p ~first ~last ~first_line ~last_line rev_spans =
    let raw = raw_tight_block_lines p ~rev_spans in
    let textloc =
      let first = Meta.textloc (snd (snd (List.hd raw))) in
      let last = snd (List.hd rev_spans) in
      let last_byte = last.last and last_line = last.line_pos in
      Textloc.set_last first ~last_byte ~last_line
    in
    let inline = Inline.Raw_html (raw, meta p textloc) in
    Inline { start = first; inline; endline = last_line; next = last + 1 }

  let link_token p ~first ~last ~first_line ~last_line ~image link =
    let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
    let link = link, meta p textloc in
    let inline = if image then Inline.Image link else Inline.Link link in
    Inline { start = first; inline; endline = last_line; next = last + 1 }

  let emphasis_token p ~first ~last ~first_line ~last_line ~strong emph =
    let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
    let delim = p.i.[first] in
    let e = { Inline.Emphasis.delim; inline = emph}, meta p textloc in
    let i = if strong then Inline.Strong_emphasis e else Inline.Emphasis e in
    Inline { start = first; inline = i ; endline = last_line; next = last + 1 }

  let ext_strikethrough_token p ~first ~last ~first_line ~last_line s =
    let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
    let inline = Inline.Ext_strikethrough (s, meta p textloc) in
    Inline { start = first; inline; endline = last_line; next = last + 1 }

  let ext_math_span_token p ~count ~first ~last ~first_line ~last_line rspans =
    let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
    let tex_layout = raw_tight_block_lines p ~rev_spans:rspans in
    let meta = meta p textloc in
    let ms = Inline.Math_span.make ~display:(count = 2) tex_layout in
    let inline = Inline.Ext_math_span (ms, meta) in
    Inline { start = first; inline; endline = last_line; next = last + 1 }

  (* Parsers *)

  let try_code p toks start_line ~start:cstart ~count ~escaped =
    (* https://spec.commonmark.org/current/#code-span *)
    if escaped || not (has_backticks ~count ~after:cstart p.cidx) then None else
    let rec match_backticks toks line ~count spans k = match toks with
    | [] -> None
    | Backticks { start; count = c; _ } :: toks ->
        if c <> count then match_backticks toks line ~count spans k else
        let span = line.first, { line with first = k; last = start - 1} in
        let spans = span :: spans in
        let first = cstart and last = start + count - 1 in
        let first_line = start_line and last_line = line in
        let t =
          code_span_token p ~count ~first ~last ~first_line ~last_line spans
        in
        Some (toks, line, t)
    | Newline { newline } :: toks ->
        let spans = (line.first, { line with first = k }) :: spans in
        let k = first_non_blank_in_span p newline in
        match_backticks toks newline ~count spans k
    | _ :: toks -> match_backticks toks line ~count spans k
    in
    let first = cstart + count in
    match_backticks toks { start_line with first } ~count [] first

  let try_math_span p toks start_line ~start:cstart ~count =
    if not (has_math_span_closer ~count ~after:cstart p.cidx) then None else
    let rec match_math_marks toks line ~count spans k = match toks with
    | [] -> None
    | Math_span_marks { start; count = c; may_close; _ } :: toks ->
        if c <> count || not may_close
        then match_math_marks toks line ~count spans k else
        let span = line.first, { line with first = k; last = start - 1 } in
        let spans = span :: spans in
        let first = cstart and last = start + count - 1 in
        let first_line = start_line and last_line = line in
        let t =
          ext_math_span_token p ~count ~first ~last ~first_line ~last_line
            spans
        in
        Some (toks, line, t)
    | Newline { newline } :: toks ->
        let spans = (line.first, { line with first = k }) :: spans in
        let k = first_non_blank_in_span p newline in
        match_math_marks toks newline ~count spans k
    | _ :: toks -> match_math_marks toks line ~count spans k
    in
    let first = cstart + count in
    match_math_marks toks { start_line with first } ~count [] first

  let try_autolink_or_html p toks line ~start =
    match Match.autolink_uri p.i ~last:line.last ~start with
    | Some last ->
        let t = autolink_token p line ~first:start ~last ~is_email:false in
        let toks = drop_until ~start:(last + 1) toks in
        Some (toks, line, t)
    | None ->
    match Match.autolink_email p.i ~last:line.last ~start with
    | Some last ->
        let t = autolink_token p line ~first:start ~last ~is_email:true in
        let toks = drop_until ~start:(last + 1) toks in
        Some (toks, line, t)
    | None ->
    match Match.raw_html ~next_line p.i toks ~line ~start with
    | None -> None
    | Some (toks, last_line, spans, last) ->
        let first = start and first_line = line in
        let t = raw_html_token p ~first ~last ~first_line ~last_line spans in
        let toks = drop_until ~start:(last + 1) toks in
        Some (toks, last_line, t)

  let label_of_rev_spans p ~key rev_spans =
    let meta =
      if p.nolocs || rev_spans = [] then Meta.none else
      let first = snd (List.hd (List.rev rev_spans)) in
      let last = snd (List.hd rev_spans) in
      meta_of_spans p ~first ~last
    in
    let text = tight_block_lines p ~rev_spans in
    { Label.meta; key; text }

  let try_full_reflink_remainder p toks line ~image ~start (* is label's [ *) =
    (* https://spec.commonmark.org/current/#full-reference-link *)
    match Match.link_label p.buf ~next_line p.i toks ~line ~start with
    | None -> None
    | Some (toks, line, rev_spans, last, key) ->
        let ref = label_of_rev_spans p ~key rev_spans in
        let toks = drop_stop_after_right_brack toks in
        match find_def_for_ref p ~image ref with
        | None -> Some None
        | Some def -> Some (Some (toks, line, `Ref (`Full, ref, def), last))

  let try_shortcut_reflink p toks line ~image ~start (* is starting [ or ! *) =
    (* https://spec.commonmark.org/current/#shortcut-reference-link *)
    let start = if image then start + 1 (* [ *) else start in
    match Match.link_label p.buf ~next_line p.i toks ~line ~start with
    | None -> None
    | Some (toks, line, rev_spans, last, key) ->
        let ref = label_of_rev_spans p ~key rev_spans in
        let toks = drop_stop_after_right_brack toks in
        match find_def_for_ref p ~image ref with
        | None -> None
        | Some def -> Some (toks, line, `Ref (`Shortcut, ref, def), last)

  let try_collapsed_reflink p toks line ~image ~start (* is starting [ or ! *) =
    (* https://spec.commonmark.org/current/#collapsed-reference-link *)
    let start = if image then start + 1 (* [ *) else start in
    match Match.link_label p.buf ~next_line p.i toks ~line ~start with
    | None -> None
    | Some (toks, line, rev_spans, last, key) ->
        let ref = label_of_rev_spans p ~key rev_spans in
        let last = last + 2 in (* adjust for ][] *)
        let toks = drop_stop_after_right_brack toks in
        let toks = drop_stop_after_right_brack toks in
        match find_def_for_ref p ~image ref with
        | None -> None
        | Some def -> Some (toks, line, `Ref (`Collapsed, ref, def), last)

  let try_inline_link_remainder p toks start_line ~image ~start:st (* is ( *) =
    (* https://spec.commonmark.org/current/#inline-link *)
    if not (has_right_paren ~after:st p.cidx) then None else
    let first_non_blank_over_nl = first_non_blank_over_nl ~next_line in
    match first_non_blank_over_nl p toks start_line ~start:(st + 1) with
    | None -> None
    | Some (toks, line, before_dest, start) ->
        let toks, line, angled_dest, dest, start =
          match Match.link_destination p.i ~last:line.last ~start with
          | None -> toks, line, false, None, start
          | Some (angled, first, last) ->
              let dest = clean_unesc_unref_span p { line with first; last } in
              let next = if angled then last + 2 else last + 1 in
              toks, line, angled, Some dest, next
        in
        let toks, line, after_dest, title_open_delim, title, start =
          match first_non_blank_over_nl p toks line ~start with
          | None ->
              toks, line, [], '\"', None, start
          | Some (_, _, _, start') when start' = start ->
              toks, line, [], '\"', None, start
          | Some (toks, line, after_destination, start) ->
              match Match.link_title ~next_line p.i toks ~line ~start with
              | None -> toks, line, after_destination, '\"', None, start
              | Some (toks, line, rev_spans, last) ->
                  let title = tight_block_lines p ~rev_spans in
                  toks, line, after_destination, p.i.[start],
                  Some title, last + 1
        in
        let toks, line, after_title, last =
          match first_non_blank_over_nl p toks line ~start with
          | None -> toks, line, [], start
          | Some (toks, line, after_title, start as v) -> v
        in
        if last > line.last || p.i.[last] <> ')' then None else
        let layout =
          { Link_definition.indent = 0; angled_dest; before_dest;
            after_dest; title_open_delim; after_title; }
        in
        let label = None and defined_label = None in
        let ld = { Link_definition.layout; label; defined_label; dest; title }in
        let textloc =
          let first = st and last = start in
          textloc_of_lines p ~first ~last ~first_line:start_line ~last_line:line
        in
        let ld = (ld, meta p textloc) in
        let toks = drop_until ~start:(last + 1) toks in
        Some (toks, line, `Inline ld, last)

  let find_link_text_tokens p toks start_line ~start =
    (* XXX The repetition with first_pass is annoying here.
       we should figure out something for that not to happen. *)
    (* https://spec.commonmark.org/current/#link-text *)
    let rec loop toks line nest acc = match toks with
    | Right_brack { start = last } :: toks when nest = 0 ->
        let acc = rev_tokens_and_shorten_last_line ~to_last:(last - 1) [] acc in
        Some (toks, line, acc, last)
    | Backticks { start; count; escaped } :: toks ->
        begin match try_code p toks line ~start ~count ~escaped with
        | None -> loop toks line nest acc
        | Some (toks, line, t) -> loop toks line nest (t :: acc)
        end
    | Math_span_marks { start; count; may_open; } :: toks ->
        if not may_open then loop toks line nest acc else
        begin match try_math_span p toks line ~start ~count with
        | None -> loop toks line nest acc
        | Some (toks, line, t) -> loop toks line nest (t :: acc)
        end
    | Autolink_or_html_start { start } :: toks ->
        begin match try_autolink_or_html p toks line ~start with
        | None -> loop toks line nest acc
        | Some (toks, line, t) -> loop toks line nest (t :: acc)
        end
    | Right_brack _ as t :: toks -> loop toks line (nest - 1) (t :: acc)
    | Link_start _  as t :: toks -> loop toks line (nest + 1) (t :: acc)
    | Newline { newline; _ } as t :: toks -> loop toks newline nest (t :: acc)
    | Inline { endline; _ } as t :: toks -> loop toks endline nest (t :: acc)
    | t :: toks -> loop toks line nest (t :: acc)
    | [] -> None
    in
    loop toks start_line 0 []

  let try_link_def
      p ~start ~start_toks ~start_line ~toks ~line ~text_last ~image text
    =
    let next = text_last + 1 in
    let link =
      if next > line.last
      then try_shortcut_reflink p start_toks start_line ~image ~start else
      match p.i.[next] with
      | '(' ->
          (match try_inline_link_remainder p toks line ~image ~start:next with
          | None -> try_shortcut_reflink p start_toks start_line ~image ~start
          | Some _ as v -> v)
      | '[' ->
          let next' = next + 1 in
          if next' <= line.last && p.i.[next'] = ']'
          then try_collapsed_reflink p start_toks start_line ~image ~start else
          let r = try_full_reflink_remainder p toks line ~image ~start:next in
          begin match r with
          | None -> try_shortcut_reflink p start_toks start_line ~image ~start
          | Some None -> None (* Example 570 *)
          | Some (Some _ as v) -> v
          end
      | c ->
          try_shortcut_reflink p start_toks start_line ~image ~start
    in
    match link with
    | None -> None
    | Some (toks, endline, reference, last) ->
        let first = start in
        let text =
          let first_line = start_line and last_line = line in
          inlines_inline p text ~first ~last:text_last ~first_line ~last_line
        in
        let link = { Inline.Link.text; reference } in
        let first_line = start_line and last_line = endline in
        let t = link_token p ~image ~first ~last ~first_line ~last_line link in
        let had_link = not image && not p.nested_links in
        Some (toks, endline, t, had_link)

  (* The following sequence of mutually recursive functions define
     inline parsing. We have three passes over a paragraph's token
     list see the [parse_tokens] function below. *)

  let rec try_link p start_toks start_line ~image ~start =
    if not (has_right_brack ~after:start p.cidx) then None else
    match find_link_text_tokens p start_toks start_line ~start with
    | None -> None
    | Some (toks, line, text_toks, text_last (* with ] delim *)) ->
        let text, had_link =
          let text_start =
            let first = start + (if image then 2 else 1) in
            let last =
              if start_line == line then text_last - 1 else start_line.last
            in
            { start_line with first; last }
          in
          parse_tokens p text_toks text_start
        in
        if had_link && not image
        then None (* Could try to keep render *) else
        try_link_def
          p ~start ~start_toks ~start_line ~toks ~line ~text_last ~image text

  and first_pass p toks line =
    (* Parse inline atoms and links. Links are parsed here otherwise
       link reference data gets parsed as atoms. *)
    let rec loop p toks line ~had_link acc = match toks with
    | [] -> List.rev acc, had_link
    | Backticks { start; count; escaped } :: toks ->
        begin match try_code p toks line ~start ~count ~escaped with
        | None -> loop p toks line ~had_link acc
        | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
        end
    | Math_span_marks { start; count; may_open; } :: toks ->
        if not may_open then loop p toks line ~had_link acc else
        begin match try_math_span p toks line ~start ~count with
        | None -> loop p toks line ~had_link acc
        | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
        end
    | Autolink_or_html_start { start } :: toks ->
        begin match try_autolink_or_html p toks line ~start with
        | None -> loop p toks line ~had_link acc
        | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
        end
    | Link_start { start; image } :: toks ->
        begin match try_link p toks line ~image ~start with
        | None -> loop p toks line ~had_link acc
        | Some (toks, line, t, had_link) ->
            loop p toks line ~had_link (t :: acc)
        end
    | Right_brack start :: toks -> loop p toks line ~had_link acc
    | Newline { newline = l } as t :: toks -> loop p toks l ~had_link (t :: acc)
    | t :: toks -> loop p toks line ~had_link (t :: acc)
    in
    loop p toks line ~had_link:false []

  (* Second pass *)

  and find_emphasis_text p toks line ~opener =
    let marks_match ~marks ~opener =
      (opener.char = marks.char) &&
      (not (marks.may_open || opener.may_close) ||
       marks.count mod 3 = 0 || (opener.count + marks.count) mod 3 != 0)
    in
    let marks_has_precedence p ~marks ~opener =
      if marks.char = opener.char (* Rule 16 *) then true else (* Rule 15 *)
      emphasis_closer_pos ~char:marks.char ~after:marks.start p.cidx <
      emphasis_closer_pos ~char:opener.char ~after:marks.start p.cidx
    in
    let rec loop p toks line acc ~opener = match toks with
    | [] -> Either.Left (List.rev acc) (* No match but keep nested work done *)
    | Emphasis_marks marks as t :: toks ->
        let after = marks.start in
        if marks.may_close && marks_match ~marks ~opener then
          let used = if marks.count >= 2 && opener.count >= 2 then 2 else 1 in
          let to_last = marks.start - 1 in
          let acc = rev_tokens_and_shorten_last_line ~to_last [] acc in
          Either.Right (toks, line, used, acc, marks)
        else if marks.may_open && marks_has_precedence p ~marks ~opener then
          match try_emphasis p toks line ~opener:marks with
          | Either.Left toks -> loop p toks line acc ~opener
          | Either.Right (toks, line) -> loop p toks line acc ~opener
        else if has_emphasis_closer ~char:opener.char ~after p.cidx then
          loop p toks line (t :: acc) ~opener
        else (Either.Left (List.rev_append (t :: acc) toks))
    | Newline { newline = l } as t :: toks -> loop p toks l (t :: acc) ~opener
    | Inline { endline = l } as t :: toks -> loop p toks l (t :: acc) ~opener
    | t :: toks -> loop p toks line (t :: acc) ~opener
    in
    loop p toks line [] ~opener

  and try_emphasis p start_toks start_line ~opener =
    let start = opener.start in
    if not (has_emphasis_closer ~char:opener.char ~after:start p.cidx)
    then Either.Left start_toks else
    match find_emphasis_text p start_toks start_line ~opener with
    | Either.Left _ as r -> r
    | Either.Right (toks, line, used, emph_toks, closer) ->
        let text_first = start + opener.count in
        let text_last = closer.start - 1 (* XXX prev line ? *) in
        let first = text_first - used in
        let last = closer.start + used - 1 in
        let first_line = start_line and last_line = line in
        let emph =
          let text_start =
            let last =
              if start_line == line then text_last else start_line.last
            in
            { start_line with first = text_first; last }
          in
          (* No need to redo first pass *)
          let emph_toks = second_pass p emph_toks text_start in
          let text = last_pass p emph_toks text_start in
          inlines_inline p text ~first ~last:text_last ~first_line ~last_line
        in
        let toks =
          let count = closer.count - used in
          if count = 0 then toks else
          Emphasis_marks { closer with start = last + 1; count } :: toks
        in
        let toks =
          let strong = used = 2 in
          emphasis_token p ~first ~last ~first_line ~last_line ~strong emph ::
          toks
        in
        let toks =
          let count = opener.count - used in
          if count = 0 then toks else
          Emphasis_marks { opener with count } :: toks
        in
        Either.Right (toks, line)

  and find_strikethrough_text p toks start_line =
    let rec loop p toks line acc = match toks with
    | [] -> Either.Left (List.rev acc) (* No match but keep nested work done *)
    | Strikethrough_marks marks :: toks ->
        if marks.may_close then
          let to_last = marks.start - 1 in
          let acc = rev_tokens_and_shorten_last_line ~to_last [] acc in
          Either.Right (toks, line, acc, marks)
        else if marks.may_open then
          match try_strikethrough p toks line ~opener:marks with
          | Either.Left toks -> loop p toks line acc
          | Either.Right (toks, line) -> loop p toks line acc
        else assert false
    | Newline { newline = l } as t :: toks -> loop p toks l (t :: acc)
    | Inline { endline = l } as t :: toks -> loop p toks l (t :: acc)
    | t :: toks -> loop p toks line (t :: acc)
    in
    loop p toks start_line []

  and try_strikethrough p start_toks start_line ~opener =
    let start = opener.start in
    if not (has_strikethrough_closer ~after:start p.cidx)
    then Either.Left start_toks else
    match find_strikethrough_text p start_toks start_line with
    | Either.Left _ as r -> r
    | Either.Right (toks, line, stroken_toks, closer) ->
        let first_line = start_line and last_line = line in
        let text =
          let first = start + 2 in
          let last = closer.start - 1 in
          let text_start =
            let last =
              if start_line == line then last else start_line.last
            in
            { start_line with first; last }
          in
          (* No need to redo first pass *)
          let emph_toks = second_pass p stroken_toks text_start in
          let text = last_pass p emph_toks text_start in
          inlines_inline p text ~first ~last ~first_line ~last_line
        in
        let toks =
          let first = opener.start and last = closer.start + 1 in
          ext_strikethrough_token p ~first ~last ~first_line ~last_line text
          :: toks
        in
        Either.Right (toks, line)

  and second_pass p toks line =
    let rec loop p toks line acc = match toks with
    | [] -> List.rev acc
    | Emphasis_marks ({ may_open } as opener) :: toks ->
        if not may_open then loop p toks line acc else
        begin match try_emphasis p toks line ~opener with
        | Either.Left toks -> loop p toks line acc
        | Either.Right (toks, line) -> loop p toks line acc
        end
    | Strikethrough_marks ({ may_open } as opener) :: toks ->
        if not may_open then loop p toks line acc else
        begin match try_strikethrough p toks line ~opener with
        | Either.Left toks -> loop p toks line acc
        | Either.Right (toks, line) -> loop p toks line acc
        end
    | Newline { newline } as t :: toks -> loop p toks newline (t :: acc)
    | Inline { endline } as t :: toks -> loop p toks endline (t :: acc)
    | t :: toks -> loop p toks line (t :: acc)
    in
    loop p toks line []

  (* Last pass *)

  and last_pass p toks line =
    (* Only [Inline] and [Newline] tokens remain. We fold over them to
       convert them to [inline] values and [Break]s. [Text] inlines
       are created for data between them. *)
    let rec loop toks line acc k = match toks with
    | [] ->
        List.rev (try_add_text_inline p line ~first:k ~last:line.last acc)
    | Newline { start; break_type; newline } :: toks ->
        let acc = try_add_text_inline p line ~first:k ~last:(start - 1) acc in
        let break = break_inline p line ~start ~break_type ~newline in
        loop toks newline (break :: acc) newline.first
    | Inline { start; inline; endline; next } :: toks ->
        let acc = try_add_text_inline p line ~first:k ~last:(start - 1) acc in
        let acc = match inline with
        | Inline.Inlines (is, _meta_stub) -> List.rev_append (List.rev is) acc
        | i -> i :: acc
        in
        loop toks endline acc next
    | (Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
      | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
      | Math_span_marks _) :: _ ->
        assert false
    in
    loop toks line [] line.first

  and parse_tokens p toks first_line =
    let toks, had_link = first_pass p toks first_line in
    let toks = second_pass p toks first_line in
    last_pass p toks first_line, had_link

  let strip_paragraph p lines =
    (* Remove initial and final blanks. Initial blank removal on
       other paragraph lines is done during the inline parsing
       and integrated into the AST for layout preservation. *)
    let last, trailing_blanks =
      let line = List.hd lines in
      let first = line.first and start = line.last in
      let non_blank = Match.last_non_blank p.i ~first ~start in
      { line with last = non_blank},
      layout_clean_raw_span' p { line with first = non_blank + 1; }
    in
    let lines = List.rev (last :: List.tl lines) in
    let first, leading_indent =
      let line = List.hd lines in
      let non_blank = first_non_blank_in_span p line in
      { line with first = non_blank },
      non_blank - line.first
    in
    let lines = first :: List.tl lines in
    let meta = meta_of_spans p ~first ~last in
    (leading_indent, trailing_blanks), meta, lines

  let parse p lines =
    let layout, meta, lines = strip_paragraph p lines in
    let cidx, toks, first_line = tokenize ~exts:p.exts p.i lines in
    p.cidx <- cidx;
    let is, _had_link = parse_tokens p toks first_line in
    let inline = match is with [i] -> i | is -> Inline.Inlines (is, meta) in
    layout, inline

  (* Parsing table rows *)

  let get_blanks p line ~before k =
    let nb = Match.first_non_blank p.i ~last:(before - 1) ~start:k in
    layout_clean_raw_span' p { line with first = k; last = nb - 1 }, nb

  let make_col p = function
  | [] -> assert false
  | [i] -> i
  | is ->
      let last = Inline.meta (List.hd is) in
      let is = List.rev is in
      let first = Inline.meta (List.hd is) in
      let meta = meta_of_metas p ~first ~last in
      Inline.Inlines (is, meta)

  let find_pipe p line ~before k =
    let text p ~first ~last =
      Inline.Text (clean_unesc_unref_span p { line with first; last })
    in
    let n = Match.first_non_escaped_char '|' p.i ~last:(before - 1) ~start:k in
    if n = before then `Not_found (text p ~first:k ~last:(n - 1)) else
    let nb = Match.last_non_blank p.i ~first:k ~start:(n - 1) in
    let after =
      layout_clean_raw_span' p { line with first = nb + 1; last = n - 1 }
    in
    let text = if nb < k then None else Some (text p ~first:k ~last:nb) in
    `Found (text, after, n + 1)

  let start_col p line ~before k =
    let bbefore, k = get_blanks p line ~before k in
    if k >= before then `Start (bbefore, []) else
    match find_pipe p line ~before k with
    | `Not_found text -> `Start (bbefore, [text])
    | `Found (text, bafter, k) ->
        let text = match text with
        | Some text -> text
        | None ->
            let l = textloc_of_span p { line with first = k; last = k - 1 }in
            (Inline.Inlines ([], meta p l))
        in
        `Col ((text, (bbefore, bafter)), k)

  let rec finish_col p line blanks_before is toks k = match toks with
  | [] ->
      begin match find_pipe p line ~before:(line.last + 1) k with
      | `Found (text, after, k) ->
          let is = match text with Some t -> t :: is | None -> is in
          (make_col p is, (blanks_before, after)), [], k
      | `Not_found _ -> assert false
      end
  | Inline { start; inline; next } :: toks when k >= start ->
      finish_col p line blanks_before (inline :: is) toks next
  | Inline { start; inline; next } :: toks as toks' ->
      begin match find_pipe p line ~before:start k with
      | `Not_found text ->
          let is = inline :: text :: is in
          finish_col p line blanks_before is toks next
      | `Found (text, after, k) ->
          let is = match text with Some t -> t :: is | None -> is in
          (make_col p is, (blanks_before, after)), toks', k
      end
  | (Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
    | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
    | Math_span_marks _ | Newline _ ) :: _ ->
      assert false

  let rec parse_cols p line acc toks k = match toks with
  | [] ->
      if k > line.last then (List.rev acc) else
      begin match start_col p line ~before:(line.last + 1) k with
      | `Col (col, k) -> parse_cols p line (col :: acc) [] k
      | `Start _ -> assert false
      end
  | Inline { start; inline; next } :: toks as toks' ->
      begin match start_col p line ~before:start k with
      | `Col (col, k) -> parse_cols p line (col :: acc) toks' k
      | `Start (before, is) ->
          let is = inline :: is in
          let col, toks, k = finish_col p line before is toks next in
          parse_cols p line (col :: acc) toks k
      end
  | (Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
    | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
    | Math_span_marks _ | Newline _ ) :: _ ->
      assert false

  let parse_table_row p line =
    let cidx, toks, first_line = tokenize ~exts:p.exts p.i [line] in
    p.cidx <- cidx;
    let toks, _had_link = first_pass p toks first_line in
    let toks = second_pass p toks first_line in
    (* We now have modified last pass, inner inlines will have gone through
       the regular [last_pass] which is fine since we are only interested
       in creating the toplevel text nodes further splited on (unescaped)
       [\]. *)
    parse_cols p line [] toks line.first
end

(* Block structure parsing. *)

module Block_struct = struct

  (* Moving on the line in the indentation space (columns) and over container
     markers. *)

  let[@inline] current_col p = p.current_char_col + p.tab_consumed_cols
  let[@inline] current_indent p = p.next_non_blank_col - current_col p
  let[@inline] end_of_line p = p.current_char > p.current_line_last_char
  let[@inline] only_blanks p = p.next_non_blank > p.current_line_last_char
  let[@inline] has_next_non_blank p =
    p.next_non_blank <= p.current_line_last_char

  let update_next_non_blank p =
    let rec loop p s last k col =
      if k > last then (p.next_non_blank <- k; p.next_non_blank_col <- col) else
      match s.[k] with
      | ' ' -> loop p s last (k + 1) (col + 1)
      | '\t' -> loop p s last (k + 1) (next_tab_stop col)
      | _ -> p.next_non_blank <- k; p.next_non_blank_col <- col;
    in
    loop p p.i p.current_line_last_char p.current_char p.current_char_col

  let accept_cols ~count p =
    let rec loop p count k col =
      if count = 0 then (p.current_char <- k; p.current_char_col <- col) else
      if p.i.[k] <> '\t' then loop p (count - 1) (k + 1) (col + 1) else
      let col' = next_tab_stop col in
      let tab_cols = col' - (col + p.tab_consumed_cols) in
      if tab_cols > count
      then (p.tab_consumed_cols <- count; loop p 0 k col)
      else (p.tab_consumed_cols <- 0; loop p (count - tab_cols) (k + 1) col')
    in
    loop p count p.current_char p.current_char_col;
    update_next_non_blank p

  let match_and_accept_block_quote p =
    (* https://spec.commonmark.org/current/#block-quote-marker *)
    if end_of_line p || p.i.[p.current_char] <> '>' then false else
    let next_is_blank =
      let next = p.current_char + 1 in
      next <= p.current_line_last_char && Ascii.is_blank p.i.[next]
    in
    let count = if next_is_blank then (* we eat a space *) 2 else 1 in
    accept_cols ~count p; true

  let accept_list_marker_and_indent p ~marker_size ~last =
    (* Returns min indent after marker for list item  *)
    accept_cols ~count:marker_size p;
    let indent = current_indent p in
    let min_indent =
      if only_blanks p || indent > 4 (* indented code *)
      then 1
      else min indent 4
    in
    accept_cols ~count:min_indent p;
    min_indent

  let accept_code_indent p ~count =
    (* Returns padding for partially consumed tab and content first char *)
    accept_cols p ~count;
    if p.tab_consumed_cols = 0 then 0, p.current_char else
    let col' = next_tab_stop p.current_char_col in
    let pad = col' - (p.current_char_col + p.tab_consumed_cols) in
    pad, p.current_char (* is '\t' *) + 1

  (* These data types are only used during parsing, to find out the
     block structure. All the lists (blocks, lines) are in reverse
     order. We don't extract data from the input here. We just store
     line spans. See:
     https://spec.commonmark.org/current/#phase-1-block-structure *)

  type space_pad = int (* number of space characters to pad content with. *)
  type indented_code_line =
    { pad : space_pad;
      code : line_span;
      is_blank : bool }

  type fence =
    { indent : Layout.indent;
      opening_fence : line_span;
      fence : Char.t * int (* fence length *);
      info_string : line_span option (* we drop the trailing blanks *);
      closing_fence : line_span option; }

  type fenced_code_block =
    { fence : fence;
      code : (space_pad * line_span) list }

  type code_block =
  [ `Indented of indented_code_line list | `Fenced of fenced_code_block ]

  type atx =
    { indent : Layout.indent;
      level : Match.heading_level;
      after_open : byte_pos;
      heading : line_span;
      layout_after : line_span }

  type setext =
    { level : Match.heading_level;
      heading_lines : line_span list;
      underline : (* Indent, underline char count, blanks *)
        Layout.indent * line_span * line_span; }

  type heading = [ `Atx of atx | `Setext of setext ]

  type html_block =
    { end_cond : Match.html_block_end_cond option;
      html : line_span list }

  type paragraph = { maybe_ref : bool; lines : line_span list }

  type t =
  | Block_quote of Layout.indent * t list
  | Blank_line of space_pad * line_span
  | Code_block of code_block
  | Heading of heading
  | Html_block of html_block
  | List of list'
  | Linkref_def of Link_definition.t node
  | Paragraph of paragraph
  | Thematic_break of Layout.indent * line_span (* including trailing blanks *)
  | Ext_table of Layout.indent * (line_span * line_span (* trail blanks *)) list
  | Ext_footnote of Layout.indent * (Label.t * Label.t option) * t list

  and list_item =
   { before_marker : Layout.indent;
     marker : line_span;
     after_marker : Layout.indent;
     ext_task_marker : (Uchar.t * line_span) option;
     blocks : t list }

  and list' =
    { last_blank : bool; (* last added line was blank and not first line
                            of item *)
      loose : bool; (* inter-item looseness, intra-item is computed later *)
      item_min_indent : int; (* last item minimal indent *)
      list_type : Block.List'.type';
      items : list_item list; }

  let block_is_blank_line = function Blank_line _ -> true | _ -> false

  (* Making blocks from the current line status *)

  let blank_line p =
    let first = p.current_char and last = p.current_line_last_char in
    Blank_line (0, current_line_span p ~first ~last)

  let thematic_break p ~indent ~last:_ =
    let last = p.current_line_last_char (* let's keep everything *) in
    let break = current_line_span p ~first:p.current_char ~last in
    Thematic_break (indent, break)

  let atx_heading p ~indent ~level ~after_open ~first_content ~last_content =
    let heading = current_line_span p ~first:first_content ~last:last_content in
    let layout_after =
      let first = last_content + 1 and last = p.current_line_last_char in
      current_line_span p ~first ~last
    in
    Heading (`Atx { indent; level; after_open; heading; layout_after })

  let setext_heading p ~indent ~level ~last_underline heading_lines =
    let u = current_line_span p ~first:p.current_char ~last:last_underline in
    let blanks =
      let first = last_underline + 1 and last = p.current_line_last_char in
      current_line_span p ~first ~last
    in
    let underline = indent, u, blanks in
    Heading (`Setext {level; heading_lines; underline})

  let indented_code_block p = (* Has a side-effect on [p] *)
    let pad, first = accept_code_indent p ~count:4 in
    let code = current_line_span p ~first ~last:p.current_line_last_char in
    Code_block (`Indented [{pad; code; is_blank = false}])

  let fenced_code_block p ~indent ~fence_first ~fence_last ~info =
    let info_string, layout_last = match info with
    | None -> None, p.current_line_last_char
    | Some (first, last) -> Some (current_line_span p ~first ~last), first - 1
    in
    let opening_fence =
      current_line_span p ~first:fence_first ~last:layout_last
    in
    let fence = p.i.[fence_first], (fence_last - fence_first + 1) in
    let closing_fence = None in
    let fence = { indent; opening_fence; fence; info_string; closing_fence } in
    Code_block (`Fenced {fence; code = []})

  let html_block p ~end_cond ~indent_start =
    let first = indent_start and last = p.current_line_last_char in
    let end_cond = (* Check if the same line matches the end condition. *)
      if Match.html_block_end p.i ~end_cond ~last ~start:p.current_char
      then None (* We are already closed *) else Some end_cond
    in
    Html_block { end_cond; html = [current_line_span p ~first ~last] }

  let paragraph p ~start =
    let last = p.current_line_last_char in
    let maybe_ref = Match.could_be_link_reference_definition p.i ~last ~start in
    Paragraph { maybe_ref; lines = [current_line_span p ~first:start ~last]}

  let add_paragraph_line p ~indent_start par bs =
    let first = indent_start and last = p.current_line_last_char in
    let lines = current_line_span p ~first ~last :: par.lines in
    Paragraph { par with lines } :: bs

  let table_row p ~first ~last =
    current_line_span p ~first ~last,
    current_line_span p ~first:(last + 1) ~last:p.current_line_last_char

  let table p ~indent ~last =
    let row = table_row p ~first:p.current_char ~last in
    Ext_table (indent, [row])

  (* Link reference definition parsing

     This is invoked when we close a paragraph and works on the paragraph
     lines. *)

  let parse_link_reference_definition p lines =
    (* Has no side effect on [p], parsing occurs on [lines] spans. *)
    (* https://spec.commonmark.org/current/#link-reference-definitions *)
    let none () = raise_notrace Exit in
    let next_line = function line :: lines -> Some (lines, line) | [] -> None in
    try
      let lines, line = match next_line lines with
      | None -> none () | Some v -> v
      in
      let start = first_non_blank_in_span p line in
      let indent = start - line.first in
      let meta_first = { line with first = start } in
      let lines, line, label, start =
        match Match.link_label p.buf ~next_line p.i lines ~line ~start with
        | None -> none ()
        | Some (lines, line, rev_spans, last, key) ->
            let colon = last + 1 in
            if colon > line.last || p.i.[colon] <> ':' then none () else
            let label = Inline_struct.label_of_rev_spans p ~key rev_spans in
            lines, line, label, colon + 1
      in
      let lines, line, before_dest, start =
        match first_non_blank_over_nl ~next_line p lines line ~start with
        | None -> none () | Some v -> v
      in
      let angled_dest, dest, start, meta_last =
        match Match.link_destination p.i ~last:line.last ~start with
        | None -> none ()
        | Some (angled, first, last) ->
            let dest = clean_unesc_unref_span p { line with first; last } in
            let next = if angled then last + 2 else last + 1 in
            angled, Some dest, next, { line with last = last }
      in
      let lines, after_dest, title_open_delim, title, after_title, meta_last =
        match first_non_blank_over_nl ~next_line p lines line ~start with
        | None -> lines, [], '\"', None, [], meta_last
        | Some (_, _, _, st) when st = start (* need some space *) -> none ()
        | Some (lines', line', after_dest, start') ->
            let no_newline = line'.line_pos = line.line_pos in
            let title =
              Match.link_title ~next_line p.i lines' ~line:line' ~start:start'
            in
            match title with
            | None ->
                if no_newline then none () (* garbage after dest *) else
                lines, [], '\"', None, [], meta_last
            | Some (lines', line', rev_spans, last) ->
                let after_title =
                  let last = line'.last and start = last + 1 in
                  let nb = Match.first_non_blank p.i ~last ~start in
                  if nb <= line'.last
                  then None
                  else
                  Some [layout_clean_raw_span p { line' with first = start; }]
                in
                match after_title with
                | None when no_newline -> none ()
                | None -> (lines, [], '\"', None, [], meta_last)
                | Some after_title ->
                    let t = tight_block_lines p ~rev_spans in
                    lines', after_dest, p.i.[start'], Some t,
                    after_title,
                    { line' with last }
      in
      let meta = meta_of_spans p ~first:meta_first ~last:meta_last in
      let layout =
        { Link_definition.indent; angled_dest; before_dest;
          after_dest; title_open_delim; after_title }
      in
      let defined_label = def_label p label in
      let label = Some label in
      let ld =
        { Link_definition.layout; label; defined_label; dest; title }, meta
      in
      begin match defined_label with
      | None -> () | Some def -> set_label_def p def (Link_definition.Def ld)
      end;
      Some (ld, lines)
    with
    | Exit -> None

  let maybe_add_link_reference_definitions p lines prevs =
    let rec loop p prevs = function
    | [] -> prevs
    | ls ->
        match parse_link_reference_definition p ls with
        | None ->
            (* Link defs can't interrupt a paragraph so we are good now. *)
            Paragraph { maybe_ref = false; lines = List.rev ls } :: prevs
        | Some (ld, ls) -> loop p (Linkref_def ld :: prevs) ls
    in
    loop p prevs (List.rev lines)

  (* Closing blocks and finishing the document. *)

  let close_indented_code_block p lines bs =
    (* Removes trailing blank lines and add them as blank lines *)
    let rec loop blanks lines bs = match lines with
    | { pad; code; is_blank = true} :: lines ->
        loop (Blank_line (pad, code) :: blanks) lines bs
    | [] -> (* likely assert (false) *) List.rev_append blanks bs
    | ls -> List.rev_append blanks ((Code_block (`Indented ls)) :: bs)
    in
    loop [] lines bs

  let close_paragraph p par bs =
    if not par.maybe_ref then Paragraph par :: bs else
    maybe_add_link_reference_definitions p par.lines bs

  let rec close_last_block p = function
  | Code_block (`Indented ls) :: bs -> close_indented_code_block p ls bs
  | Paragraph par :: bs -> close_paragraph p par bs
  | List l :: bs -> close_list p l bs
  | Ext_footnote (i, l, blocks) :: bs -> close_footnote p i l blocks bs
  | bs -> bs

  and close_list p l bs =
    let i = List.hd l.items in
    let blocks = close_last_block p i.blocks in
    (* The final blank line extraction of the list item entails less blank
       line churn for CommonMark rendering but we don't do it on empty list
       items.  *)
    match blocks with
    | Blank_line _ as bl :: (_ :: _ as blocks) ->
        let items = { i with blocks } :: List.tl l.items in
        bl :: List { l with items } :: bs
    | blocks ->
        let items = { i with blocks } :: List.tl l.items in
        List { l with items } :: bs

  and close_footnote p indent label blocks bs =
    let blocks = close_last_block p blocks in
    (* Like for lists above we do blank line extraction (except if blocks
       is only a blank line) *)
    let blanks, blocks =
      let rec loop acc = function
      | Blank_line _ as bl :: (_ :: _ as blocks) -> loop (bl :: acc) blocks
      | blocks -> acc, blocks
      in
      loop [] blocks
    in
    List.rev_append blanks (Ext_footnote (indent, label, blocks) :: bs)

  let close_last_list_item p l =
    let item = List.hd l.items in
    let item = { item with blocks = close_last_block p item.blocks } in
    { l with items = item :: List.tl l.items }

  let end_doc_close_fenced_code_block p fenced bs = match fenced.code with
  | (_, l) :: code when l.first > l.last (* empty line *) ->
      Blank_line (0, l) :: Code_block (`Fenced { fenced with code }) :: bs
  | _ -> Code_block (`Fenced fenced) :: bs

  let end_doc_close_html p h bs = match h.html with
  | l :: html when l.first > l.last (* empty line *) ->
      Blank_line (0, l) :: Html_block { end_cond = None; html } :: bs
  | _ ->
      Html_block { h with end_cond = None } :: bs

  let rec end_doc p = function
  | Block_quote (indent, bq) :: bs -> Block_quote (indent, end_doc p bq) :: bs
  | List list :: bs -> close_list p list bs
  | Paragraph par :: bs -> close_paragraph p par bs
  | Code_block (`Indented ls) :: bs -> close_indented_code_block p ls bs
  | Code_block (`Fenced f) :: bs -> end_doc_close_fenced_code_block p f bs
  | Html_block html :: bs -> end_doc_close_html p html bs
  | Ext_footnote (i, l, blocks) :: bs -> close_footnote p i l blocks bs
  | (Thematic_break _ | Heading _ | Blank_line _ | Linkref_def _
    | Ext_table _ ) :: _ | [] as bs -> bs

  (* Adding lines to blocks *)

  let match_line_type ~no_setext ~indent p =
    (* Effects on [p]'s column advance *)
    if only_blanks p then Match.Blank_line else
    if indent >= 4 then Indented_code_block_line else begin
      accept_cols ~count:indent p;
      if end_of_line p then Match.Blank_line else
      let start = p.current_char and last = p.current_line_last_char in
      match p.i.[start] with
      (* Early dispatch shaves a few ms but may not be worth doing vs
         testing all the cases in sequences.  *)
      | '>' ->
          if match_and_accept_block_quote p then Match.Block_quote_line else
          Paragraph_line
      | '=' when not no_setext ->
          let r = Match.setext_heading_underline p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '-' ->
          let r =
            if no_setext then Match.Nomatch else
            Match.setext_heading_underline p.i ~last ~start
          in
          if r <> Nomatch then r else
          let r = Match.thematic_break p.i ~last ~start in
          if r <> Nomatch then r else
          let r = Match.list_marker p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '#' ->
          let r = Match.atx_heading p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '+' | '*' | '0' .. '9' ->
          let r = Match.thematic_break p.i ~last ~start in
          if r <> Nomatch then r else
          let r = Match.list_marker p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '_' ->
          let r = Match.thematic_break p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '~' | '`' ->
          let r = Match.fenced_code_block_start p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '<' ->
          let r = Match.html_block_start p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '|' when p.exts ->
          let r = Match.ext_table_row p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '[' when p.exts ->
          let line_pos = p.current_line_pos in
          let r = Match.ext_footnote_label p.buf p.i ~line_pos ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | _ ->
          Paragraph_line
    end

  let list_marker_can_interrupt_paragraph p = function
  | `Ordered (1, _), marker_last | `Unordered _, marker_last ->
      let last = p.current_line_last_char and start = marker_last + 1 in
      let non_blank = Match.first_non_blank p.i ~last ~start in
      non_blank <= p.current_line_last_char (* line is not blank *)
  | _ -> false

  let same_list_type t0 t1 = match t0, t1 with
  | `Ordered (_, c0), `Ordered (_, c1)
  | `Unordered c0, `Unordered c1 when Char.equal c0 c1 -> true
  | _ -> false

  let rec add_open_blocks_with_line_class p ~indent_start ~indent bs = function
  | Match.Blank_line -> blank_line p :: bs
  | Indented_code_block_line -> indented_code_block p :: bs
  | Block_quote_line -> Block_quote (indent, add_open_blocks p []) :: bs
  | Thematic_break_line last -> thematic_break p ~indent ~last :: bs
  | List_marker_line m -> list p ~indent m bs
  | Atx_heading_line (level, after_open, first_content, last_content) ->
      atx_heading p ~indent ~level ~after_open ~first_content ~last_content ::
      bs
  | Fenced_code_block_line (fence_first, fence_last, info) ->
      fenced_code_block p ~indent ~fence_first ~fence_last ~info :: bs
  | Html_block_line end_cond -> html_block p ~end_cond ~indent_start :: bs
  | Paragraph_line -> paragraph p ~start:indent_start :: bs
  | Ext_table_row last -> table p ~indent ~last :: bs
  | Ext_footnote_label (rev_spans, last, key) ->
      footnote p ~indent ~last rev_spans key :: bs
  | Setext_underline_line _ | Nomatch ->
      (* This function should be called with a line type that comes out
         of match_line_type ~no_setext:true *)
      assert false

  and add_open_blocks p bs =
    let indent_start = p.current_char and indent = current_indent p in
    let ltype = match_line_type ~no_setext:true ~indent p in
    add_open_blocks_with_line_class p ~indent_start ~indent bs ltype

  and footnote p ~indent ~last rev_spans key =
    let label = Inline_struct.label_of_rev_spans p ~key rev_spans in
    let defined_label = match def_label p label with
    | None -> None
    | Some def as l -> set_label_def p def (Block.Footnote.stub label l); l
    in
    accept_cols p ~count:(last - p.current_char + 1);
    Ext_footnote (indent, (label, defined_label), add_open_blocks p [])

  and list_item ~indent p (list_type, last) =
    let before_marker = indent and marker_size = last - p.current_char + 1 in
    let marker = current_line_span p ~first:p.current_char ~last in
    let after_marker = accept_list_marker_and_indent p ~marker_size ~last in
    let ext_task_marker, ext_task_marker_size = match p.exts with
    | false -> None, 0
    | true ->
        let start = p.current_char and last = p.current_line_last_char in
        match Match.ext_task_marker p.i ~last ~start with
        | None -> None, 0
        | Some (u, last) ->
            accept_cols p ~count:(last - start + 1);
            let last = match last = p.current_line_last_char with
            | true -> (* empty line *) last
            | false -> (* remove space for locs *) last - 1
            in
            Some (u, current_line_span p ~first:start ~last), 4
    in
    let min = indent + marker_size + after_marker + ext_task_marker_size in
    min, { before_marker; marker; after_marker; ext_task_marker;
           blocks = add_open_blocks p [] }

  and list ~indent p (list_type, _ as m) bs =
    let item_min_indent, item = list_item ~indent p m in
    List { last_blank = false; loose = false;
           item_min_indent; list_type; items = [item] } :: bs

  let try_add_to_list ~indent p (lt, _ as m) l bs =
    let item_min_indent, item = list_item ~indent p m in
    if same_list_type lt l.list_type then
      let l = close_last_list_item p l and last_blank = false in
      let list_type = l.list_type in
      List { last_blank; loose = l.last_blank; item_min_indent; list_type;
             items = item :: l.items } :: bs
    else
    let bs = close_list p l bs and last_blank = false in
    List { last_blank; loose = false; item_min_indent; list_type = lt;
           items = [item] } :: bs

  let try_add_to_paragraph p par bs =
    let indent_start = p.current_char and indent = current_indent p in
    match match_line_type ~no_setext:false ~indent p with
    (* These can't interrupt paragraphs *)
    | Html_block_line `End_blank_7
    | Indented_code_block_line
    | Ext_table_row _ | Ext_footnote_label _
    | Paragraph_line ->
        add_paragraph_line p ~indent_start par bs
    | List_marker_line m when not (list_marker_can_interrupt_paragraph p m) ->
        add_paragraph_line p ~indent_start par bs
    | Blank_line ->
        blank_line p :: close_paragraph p par bs
    | Block_quote_line ->
        Block_quote (indent, add_open_blocks p []) :: (close_paragraph p par bs)
    | Setext_underline_line (level, last_underline) ->
        let bs = close_paragraph p par bs in
        begin match bs with
        | Paragraph { lines; _ } :: bs ->
            setext_heading p ~indent ~level ~last_underline lines :: bs
        | bs -> paragraph p ~start:indent_start :: bs
        end
    | Thematic_break_line last ->
        thematic_break p ~indent ~last :: (close_paragraph p par bs)
    | List_marker_line m ->
        list p ~indent m (close_paragraph p par bs)
    | Atx_heading_line (level, after_open, first_content, last_content) ->
        let bs = close_paragraph p par bs in
        atx_heading p ~indent ~level ~after_open ~first_content ~last_content ::
        bs
    | Fenced_code_block_line (fence_first, fence_last, info) ->
        let bs = close_paragraph p par bs in
        fenced_code_block p ~indent ~fence_first ~fence_last ~info :: bs
    | Html_block_line end_cond ->
        html_block p ~end_cond ~indent_start :: (close_paragraph p par bs)
    | Nomatch -> assert false

  let try_add_to_indented_code_block p ls bs =
    if current_indent p < 4 then
      if has_next_non_blank p
      then add_open_blocks p (close_indented_code_block p ls bs) else
      (* Blank but white is not data, make an empty span *)
      let first = p.current_line_last_char + 1 in
      let last = p.current_line_last_char in
      let code = current_line_span p ~first ~last in
      let l = { pad = 0; code; is_blank = true } in
      Code_block (`Indented (l :: ls)) :: bs
    else
    let pad, first = accept_code_indent p ~count:4 in
    let last = p.current_line_last_char in
    let is_blank = only_blanks p in
    let l = { pad; code = current_line_span p ~first ~last; is_blank } in
    Code_block (`Indented (l :: ls)) :: bs

  let try_add_to_fenced_code_block p f bs = match f with
  | { fence = { closing_fence = Some _; _}; _ } -> (* block is closed *)
      add_open_blocks p ((Code_block (`Fenced f)) :: bs)
  | { fence = { indent; fence; _} ; code = ls} as b ->
      let start = p.current_char and last = p.current_line_last_char in
      match Match.fenced_code_block_continue ~fence p.i ~last ~start with
      | `Code ->
          let strip = Int.min indent (current_indent p) in
          let pad, first = accept_code_indent p ~count:strip in
          let code = (pad, current_line_span p ~first ~last) :: ls in
          Code_block (`Fenced { b with code }) :: bs
      | `Close (first, _fence_last) ->
          let close = current_line_span p ~first ~last (* with layout *)in
          let fence = { b.fence with closing_fence = Some close } in
          Code_block (`Fenced { b with fence }) :: bs

  let try_add_to_html_block p b bs = match b.end_cond with
  | None -> add_open_blocks p (Html_block { b with end_cond = None} :: bs)
  | Some end_cond ->
      let start = p.current_char and last = p.current_line_last_char in
      let l = current_line_span p ~first:start ~last in
      if not (Match.html_block_end p.i ~end_cond ~last ~start)
      then Html_block { b with html = l :: b.html } :: bs else
      match end_cond with
      | `End_blank | `End_blank_7 ->
          blank_line p :: Html_block { b with end_cond = None } :: bs
      | _ ->
          Html_block { end_cond = None; html = l :: b.html } :: bs

  let rec try_lazy_continuation p ~indent_start = function
  | Paragraph par :: bs -> Some (add_paragraph_line p ~indent_start par bs)
  | Block_quote (indent, bq) :: bs ->
      begin match try_lazy_continuation p ~indent_start bq with
      | None -> None
      | Some bq -> Some (Block_quote (indent, bq) :: bs)
      end
  | List l :: bs ->
      let i = List.hd l.items in
      begin match try_lazy_continuation p ~indent_start i.blocks with
      | None -> None
      | Some blocks ->
          let items = { i with blocks } :: (List.tl l.items) in
          Some (List { l with items; last_blank = false } :: bs)
      end
  | _ -> None

  let try_add_to_table p ind rows bs =
    let indent_start = p.current_char and indent = current_indent p in
    match match_line_type ~indent ~no_setext:true p with
    | Ext_table_row last ->
        let row = table_row p ~first:p.current_char ~last in
        Ext_table (ind, row :: rows) :: bs
    | ltype ->
        let bs = Ext_table (ind, rows) :: bs in
        add_open_blocks_with_line_class p ~indent ~indent_start bs ltype

  let rec try_add_to_block_quote p indent_layout bq bs =
    let indent_start = p.current_char and indent = current_indent p in
    match match_line_type ~indent ~no_setext:true p with
    | Block_quote_line -> Block_quote (indent_layout, add_line p bq) :: bs
    | (Indented_code_block_line (* Looks like a *) | Paragraph_line) as ltype ->
        begin match try_lazy_continuation p ~indent_start bq with
        | Some bq -> Block_quote (indent_layout, bq) :: bs
        | None ->
            let bs = Block_quote (indent_layout, close_last_block p bq) :: bs in
            add_open_blocks_with_line_class p ~indent ~indent_start bs ltype
        end
    | ltype ->
        let bs = Block_quote (indent_layout, close_last_block p bq) :: bs in
        add_open_blocks_with_line_class p ~indent ~indent_start bs ltype

  and try_add_to_footnote p fn_indent label blocks bs =
    let indent_start = p.current_char and indent = current_indent p in
    if indent < fn_indent + 1 (* position of ^ *) then begin
      match match_line_type ~indent ~no_setext:true p with
      | (Indented_code_block_line (* Looks like a *) | Paragraph_line) as lt ->
          begin match try_lazy_continuation p ~indent_start blocks with
          | Some blocks -> Ext_footnote (fn_indent, label, blocks) :: bs
          | None ->
              let blocks = close_last_block p blocks in
              let bs = (close_footnote p fn_indent label blocks) bs in
              add_open_blocks_with_line_class p ~indent ~indent_start bs lt
          end
      | Blank_line ->
          Ext_footnote (fn_indent, label, add_line p blocks) :: bs
      | ltype ->
          let blocks = close_last_block p blocks in
          let bs = close_footnote p fn_indent label blocks bs in
          add_open_blocks_with_line_class p ~indent ~indent_start bs ltype
    end else begin
      accept_cols p ~count:(fn_indent + 1);
      Ext_footnote (fn_indent, label, add_line p blocks) :: bs
    end

  and try_add_to_list_item p list bs =
    let indent_start = p.current_char and indent = current_indent p in
    if indent >= list.item_min_indent then begin
      let last_blank = only_blanks p in
      let item = List.hd list.items and items = List.tl list.items in
      if list.last_blank && not last_blank &&
         List.for_all block_is_blank_line item.blocks
      then
         (* Item can only start with a single blank line, if we are
            here it's not a new item so the list ends *)
        add_open_blocks p (List list :: bs)
      else begin
        accept_cols ~count:list.item_min_indent p;
        let item = { item with blocks = add_line p item.blocks } in
        List { list with items = item :: items; last_blank } :: bs
      end
    end else match match_line_type ~indent ~no_setext:true p with
    | Blank_line ->
        let item = List.hd list.items and items = List.tl list.items in
        let item = { item with blocks = add_line p item.blocks } in
        List { list with items = item :: items; last_blank = true } :: bs
    | Indented_code_block_line | Paragraph_line as ltype  ->
        let item = List.hd list.items and items = List.tl list.items in
        begin match try_lazy_continuation p ~indent_start item.blocks with
        | Some blocks ->
            let items = { item with blocks } :: items in
            List { list with items; last_blank = false } :: bs
        | None ->
            let bs = close_list p list bs in
            add_open_blocks_with_line_class p ~indent ~indent_start bs ltype
        end
    | List_marker_line m ->
        try_add_to_list p ~indent m list bs
    | ltype ->
        let bs = close_list p list bs in
        add_open_blocks_with_line_class p ~indent ~indent_start bs ltype

  and add_line p = function
  | Paragraph par :: bs -> try_add_to_paragraph p par bs
  | ((Thematic_break _ | Heading _ | Blank_line _ | Linkref_def _) :: _)
  | [] as bs -> add_open_blocks p bs
  | List list :: bs -> try_add_to_list_item p list bs
  | Code_block (`Indented ls) :: bs -> try_add_to_indented_code_block p ls bs
  | Code_block (`Fenced f) :: bs -> try_add_to_fenced_code_block p f bs
  | Block_quote (ind, bq) :: bs -> try_add_to_block_quote p ind bq bs
  | Html_block html :: bs -> try_add_to_html_block p html bs
  | Ext_table (ind, rows) :: bs -> try_add_to_table p ind rows bs
  | Ext_footnote (i, l, blocks) :: bs -> try_add_to_footnote p i l blocks bs

  (* Parsing *)

  let get_first_line p =
    let max = String.length p.i - 1 in
    let k = ref 0 in
    let last_char =
      while !k <= max && p.i.[!k] <> '\n' && p.i.[!k] <> '\r' do incr k done;
      !k - 1 (* if the line is empty we have -1 *)
    in
    p.current_line_last_char <- last_char;
    update_next_non_blank p;
    (* Return first used newline (or "\n" if there is none) *)
    if !k > max || p.i.[!k] = '\n' then "\n" else
    let next = !k + 1 in
    if next <= max && p.i.[next] = '\n' then "\r\n" else "\r"

  let get_next_line p =
    let max = String.length p.i - 1 in
    if p.current_line_last_char = max then false else
    let first_char =
      let nl = p.current_line_last_char + 1 in
      if p.i.[nl] = '\n' then nl + 1 else (* assert (p.i.[nl] = '\r') *)
      let next = nl + 1 in
      if next <= max && p.i.[next] = '\n' then next + 1 else next
    in
    let last_char =
      let k = ref first_char in
      while !k <= max && p.i.[!k] <> '\n' && p.i.[!k] <> '\r' do incr k done;
      !k - 1 (* if the line is empty we have last_char = first_char - 1 *)
    in
    p.current_line_pos <- (fst p.current_line_pos + 1), first_char;
    p.current_line_last_char <- last_char;
    p.current_char <- first_char;
    p.current_char_col <- 0;
    p.tab_consumed_cols <- 0;
    update_next_non_blank p;
    true

  let parse p =
    let meta p =
      let first_byte = 0 and last_byte = p.current_line_last_char in
      let first_line = 1, first_byte and last_line = p.current_line_pos in
      let file = p.file in
      meta p (Textloc.v ~file ~first_byte ~last_byte ~first_line ~last_line)
    in
    let rec loop p bs =
      let bs = add_line p bs in
      if get_next_line p then loop p bs else (end_doc p bs), meta p
    in
    let nl = get_first_line p in
    nl, loop p []
end

(* Building the final AST, invokes inline parsing. *)

let block_struct_to_blank_line p pad span =
  Block.Blank_line (clean_raw_span p ~pad span)

let block_struct_to_code_block p = function
| `Indented (ls : Block_struct.indented_code_line list) (* non-empty *) ->
    let line p { Block_struct.pad; code; _} = clean_raw_span ~pad p code in
    let layout = `Indented and info_string = None in
    let last = (List.hd ls).code in
    let code = List.rev_map (line p) ls in
    let meta =
      let last_line = last.line_pos and last_byte = last.last in
      let start = Meta.textloc (snd (List.hd code)) in
      meta p (Textloc.set_last start ~last_byte ~last_line)
    in
    Block.Code_block ({layout; info_string; code}, meta)
| `Fenced { Block_struct.fence; code = ls } ->
    let layout =
      let opening_fence = layout_clean_raw_span p fence.opening_fence in
      let closing_fence =
        Option.map (layout_clean_raw_span p) fence.closing_fence
      in
      { Block.Code_block.indent = fence.indent; opening_fence; closing_fence }
    in
    let info_string = Option.map (clean_unesc_unref_span p) fence.info_string in
    let code = List.rev_map (fun (pad, l) -> clean_raw_span p ~pad l) ls in
    let meta =
      let first = fence.opening_fence in
      let last = match fence.closing_fence with
      | Some last -> last
      | None -> match ls with [] -> first | (_, last_line) :: _ -> last_line
      in
      meta_of_spans p ~first ~last
    in
    let cb = {Block.Code_block.layout = `Fenced layout; info_string; code} in
    if p.exts && Block.Code_block.is_math_block info_string
    then Block.Ext_math_block (cb, meta)
    else Block.Code_block (cb, meta)

let block_struct_to_heading p = function
| `Atx { Block_struct.indent; level; after_open; heading; layout_after } ->
    let after_opening =
      let first = after_open and last = heading.first - 1 in
      layout_clean_raw_span' p { heading with first; last }
    in
    let closing = layout_clean_raw_span' p layout_after in
    let layout = `Atx { Block.Heading.indent; after_opening; closing } in
    let meta =
      meta p (textloc_of_span p { heading with first = after_open - level })
    in
    let _layout, inline = Inline_struct.parse p [heading] in
    let id = match p.heading_auto_ids with
    | false -> None
    | true -> Some (`Auto (Inline.id ~buf:p.buf inline))
    in
    Block.Heading ({layout; level; inline; id}, meta)
| `Setext { Block_struct.level; heading_lines; underline } ->
    let (leading_indent, trailing_blanks), inline =
      Inline_struct.parse p heading_lines
    in
    let underline_indent, u, blanks = underline in
    let underline_blanks = layout_clean_raw_span' p blanks in
    let underline_count = u.last - u.first + 1, meta p (textloc_of_span p u) in
    let layout =
      { Block.Heading.leading_indent; trailing_blanks; underline_indent;
        underline_count; underline_blanks }
    in
    let meta =
      let last_line = u.line_pos and last_byte = u.last in
      let start = Meta.textloc (Inline.meta inline) in
      meta p (Textloc.set_last start ~last_byte ~last_line)
    in
    let id = match p.heading_auto_ids with
    | false -> None
    | true -> Some (`Auto (Inline.id ~buf:p.buf inline))
    in
    Block.Heading ({ layout = `Setext layout; level; inline; id }, meta)

let block_struct_to_html_block p (b : Block_struct.html_block) =
  let last = List.hd b.html in
  let last_byte = last.last and last_line = last.line_pos in
  let lines = List.rev_map (clean_raw_span p) b.html in
  let start_loc = Meta.textloc (snd (List.hd lines)) in
  let meta = meta p (Textloc.set_last start_loc ~last_byte ~last_line) in
  Block.Html_block (lines, meta)

let block_struct_to_paragraph p par =
  let layout, inline = Inline_struct.parse p par.Block_struct.lines in
  let leading_indent, trailing_blanks = layout in
  let meta = Inline.meta inline in
  Block.Paragraph ({ leading_indent; inline; trailing_blanks }, meta)

let block_struct_to_thematic_break p indent span =
  let layout, meta = (* not layout because of loc *) clean_raw_span p span in
  Block.Thematic_break ({ indent; layout }, meta)

let block_struct_to_table p indent rows =
  let rec loop p col_count last_was_sep acc = function
  | (row, blanks) :: rs ->
      let meta = meta p (textloc_of_span p row) in
      let row' = { row with first = row.first + 1; last = row.last } in
      let cols = Inline_struct.parse_table_row p row' in
      let col_count = Int.max col_count (List.length cols) in
      let r, last_was_sep = match Block.Table.parse_sep_row cols with
      | Some seps -> ((`Sep seps), meta), true
      | None ->
          ((if last_was_sep then `Header cols else `Data cols), meta), false
      in
      let acc = (r, layout_clean_raw_span' p blanks) :: acc in
      if rs = [] then row, col_count, acc else
      loop p col_count last_was_sep acc rs
  | [] -> assert false
  in
  let last = fst (List.hd rows) in
  let first, col_count, rows = loop p 0 false [] rows in
  let meta = meta_of_spans p ~first ~last in
  Block.Ext_table ({ indent; col_count; rows }, meta)

let rec block_struct_to_block_quote p indent bs =
  let add_block p acc b = block_struct_to_block p b :: acc in
  let last = block_struct_to_block p (List.hd bs) in
  let block = List.fold_left (add_block p) [last] (List.tl bs) in
  let block = match block with
  | [b] -> b
  | quote ->
      let first = Block.meta (List.hd quote) and last = Block.meta last in
      Block.Blocks (quote, meta_of_metas p ~first ~last)
  in
  Block.Block_quote ({indent; block}, Block.meta block)

and block_struct_to_footnote_definition p indent (label, defined_label) bs =
  let add_block p acc b = block_struct_to_block p b :: acc in
  let last = block_struct_to_block p (List.hd bs) in
  let block = List.fold_left (add_block p) [last] (List.tl bs) in
  let last = Block.meta last in
  let block = match block with
  | [b] -> b
  | bs ->
      let first = Block.meta (List.hd bs) in
      Block.Blocks (bs, meta_of_metas p ~first ~last)
  in
  let loc =
    let labelloc = Label.textloc label in
    let lastloc = Meta.textloc last in
    let loc = Textloc.span labelloc lastloc in
    let first_byte = Textloc.first_byte loc - 1 in
    Textloc.set_first loc ~first_byte ~first_line:(Textloc.first_line loc)
  in
  let fn = { Block.Footnote.indent; label; defined_label; block }, meta p loc in
  begin match defined_label with
  | None -> () | Some def -> set_label_def p def (Block.Footnote.Def fn)
  end;
  Block.Ext_footnote_definition fn

and block_struct_to_list_item p (i : Block_struct.list_item) =
  let rec loop bstate tight acc = function
  | Block_struct.Blank_line _ as bl :: bs ->
      let bstate = if bstate = `Trail_blank then `Trail_blank else `Blank in
      loop bstate tight (block_struct_to_block p bl :: acc) bs
  | Block_struct.List
      { items = { blocks = Block_struct.Blank_line _ :: _ } :: _ } as l :: bs
    ->
      loop bstate false (block_struct_to_block p l :: acc) bs
  | b :: bs ->
      let tight = tight && not (bstate = `Blank)  in
      loop `Non_blank tight (block_struct_to_block p b :: acc) bs
  | [] -> tight, acc
  in
  let last_meta, (tight, blocks) = match i.blocks with
  | [Block_struct.Blank_line _ as blank] ->
      let bl = block_struct_to_block p blank in
      Block.meta bl, (true, [bl])
  | Block_struct.Blank_line _ as blank :: bs ->
      let bl = block_struct_to_block p blank in
      (Block.meta bl), loop `Trail_blank true [bl] bs
  | b :: bs ->
      let b = block_struct_to_block p b in
      (Block.meta b), loop `Non_blank true [b] bs
  | [] -> assert false
  in
  let block = match blocks with
  | [i] -> i
  | is ->
      let first = Block.meta (List.hd is) in
      Block.Blocks (is, meta_of_metas p ~first ~last:last_meta)
  in
  let before_marker = i.before_marker and after_marker = i.after_marker in
  let marker = (* not layout to get loc *) clean_raw_span p i.marker in
  let ext_task_marker = match i.ext_task_marker with
  | None -> None
  | Some (u, span) -> Some (u, meta p (textloc_of_span p span))
  in
  let meta = meta_of_metas p ~first:(snd marker) ~last:last_meta in
  let i =
    { Block.List_item.before_marker; marker; after_marker; block;
      ext_task_marker }
  in
  (i, meta), tight

and block_struct_to_list p list =
  let rec loop p tight acc = function
  | [] -> tight, acc
  | item :: items ->
      let item, item_tight = block_struct_to_list_item p item in
      loop p (tight && item_tight) (item :: acc) items
  in
  let items = list.Block_struct.items in
  let last, tight = block_struct_to_list_item p (List.hd items) in
  let tight, items = loop p (not list.loose && tight) [last] (List.tl items) in
  let meta = meta_of_metas p ~first:(snd (List.hd items)) ~last:(snd last) in
  Block.List ({ type' = list.Block_struct.list_type; tight; items }, meta)

and block_struct_to_block p = function
| Block_struct.Block_quote (ind, bs) -> block_struct_to_block_quote p ind bs
| Block_struct.List list -> block_struct_to_list p list
| Block_struct.Paragraph par -> block_struct_to_paragraph p par
| Block_struct.Thematic_break (i, br) -> block_struct_to_thematic_break p i br
| Block_struct.Code_block cb -> block_struct_to_code_block p cb
| Block_struct.Heading h -> block_struct_to_heading p h
| Block_struct.Html_block html -> block_struct_to_html_block p html
| Block_struct.Blank_line (pad, span) -> block_struct_to_blank_line p pad span
| Block_struct.Linkref_def r -> Block.Link_reference_definition r
| Block_struct.Ext_table (i, rows) -> block_struct_to_table p i rows
| Block_struct.Ext_footnote (i, labels, bs) ->
    block_struct_to_footnote_definition p i labels bs

let block_struct_to_doc p (doc, meta) =
  match List.rev_map (block_struct_to_block p) doc with
  | [b] -> b | bs -> Block.Blocks (bs, meta)

(* Documents *)

module Doc = struct
  type t = { nl : Layout.string; block : Block.t; defs : Label.defs }
  let make ?(nl = "\n") ?(defs = Label.Map.empty) block = { nl; block; defs }
  let empty = make (Block.Blocks ([], Meta.none))
  let nl d = d.nl
  let block d = d.block
  let defs d = d.defs
  let of_string
      ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs ?file
      ?(strict = true) s
    =
    let p =
      parser ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs
        ?file ~strict s
    in
    let nl, doc = Block_struct.parse p in
    let block = block_struct_to_doc p doc in
    make ~nl block ~defs:p.defs

  let unicode_version = Cmarkit_data.unicode_version
  let commonmark_version = "0.30"
end

(* Maps and folds *)

module Mapper = struct
  type 'a filter_map = 'a option
  type 'a result = [ `Default | `Map of 'a filter_map ]
  let default = `Default
  let delete = `Map None
  let ret v = `Map (Some v)

  type t =
    { inline_ext_default : Inline.t map;
      block_ext_default : Block.t map;
      inline : Inline.t mapper;
      block : Block.t mapper }
  and 'a map = t -> 'a -> 'a filter_map
  and 'a mapper = t -> 'a -> 'a result

  let none _ _ = `Default
  let ext_inline_none _ _ = invalid_arg Inline.err_unknown
  let ext_block_none _ _ = invalid_arg Block.err_unknown
  let make
      ?(inline_ext_default = ext_inline_none)
      ?(block_ext_default = ext_block_none)
      ?(inline = none) ?(block = none) ()
    =
    { inline_ext_default; block_ext_default; inline; block }

  let inline_mapper m = m.inline
  let block_mapper m = m.block
  let inline_ext_default m = m.inline_ext_default
  let block_ext_default m = m.block_ext_default

  let ( let* ) = Option.bind

  let rec map_inline m i = match m.inline m i with
  | `Map i -> i
  | `Default ->
      let open Inline in
      match i with
      | Autolink _ | Break _ | Code_span _ | Raw_html _
      | Text _ | Ext_math_span _ as i -> Some i
      | Image (l, meta) ->
          let text = Option.value ~default:Inline.empty (map_inline m l.text) in
          Some (Image ({ l with text }, meta))
      | Link (l, meta) ->
          let* text = map_inline m l.text in
          Some (Link ({ l with text }, meta))
      | Emphasis (e, meta) ->
          let* inline = map_inline m e.inline in
          Some (Emphasis ({ e with inline }, meta))
      | Strong_emphasis (e, meta) ->
          let* inline = map_inline m e.inline in
          Some (Strong_emphasis ({ e with inline}, meta))
      | Inlines (is, meta) ->
          (match List.filter_map (map_inline m) is with
          | [] -> None | is -> Some (Inlines (is, meta)))
      | Ext_strikethrough (s, meta) ->
          let* inline = map_inline m s in
          Some (Ext_strikethrough (inline, meta))
      | ext -> m.inline_ext_default m ext

  let rec map_block m b = match m.block m b with
  | `Map b -> b
  | `Default ->
      let open Block in
      match b with
      | Blank_line _ | Code_block _ | Html_block _
      | Link_reference_definition _ | Thematic_break _
      | Ext_math_block _ as b -> Some b
      | Heading (h, meta) ->
          let inline = match map_inline m (Block.Heading.inline h) with
          | None -> (* Can be empty *) Inline.Inlines ([], Meta.none)
          | Some i -> i
          in
          Some (Heading ({ h with inline}, meta))
      | Block_quote (b, meta) ->
          let block = match map_block m b.block with
          | None -> (* Can be empty *) Blocks ([], Meta.none) | Some b -> b
          in
          Some (Block_quote ({ b with block}, meta))
      | Blocks (bs, meta) ->
          (match List.filter_map (map_block m) bs with
          | [] -> None | bs -> Some (Blocks (bs, meta)))
      | List (l, meta) ->
          let map_list_item m (i, meta) =
            let* block = map_block m (List_item.block i) in
            Some ({ i with block }, meta)
          in
          (match List.filter_map (map_list_item m) l.items with
          | [] -> None | items -> Some (List ({ l with items }, meta)))
      | Paragraph (p, meta) ->
          let* inline = map_inline m (Paragraph.inline p) in
          Some (Paragraph ({ p with inline }, meta))
      | Ext_table (t, meta) ->
          let map_col m (i, layout) = match map_inline m i with
          | None -> None | Some i -> Some (i, layout)
          in
          let map_row (((r, meta), blanks) as row) = match r with
          | `Header is ->
              (`Header (List.filter_map (map_col m) is), meta), blanks
          | `Sep _ -> row
          | `Data is ->
              (`Data (List.filter_map (map_col m) is), meta), blanks
          in
          let rows = List.map map_row t.rows in
          Some (Ext_table ({ t with Table.rows }, meta))
      | Ext_footnote_definition (fn, meta) ->
          let block = match map_block m fn.block with
          | None -> (* Can be empty *) Blocks ([], Meta.none) | Some b -> b
          in
          Some (Ext_footnote_definition ({ fn with block}, meta))
      | ext -> m.block_ext_default m ext

  let map_doc m d =
    let map_block m b = Option.value ~default:Block.empty (map_block m b) in
    (* XXX something better for defs should be devised here. *)
    let map_def m = function
    | Block.Footnote.Def (fn, meta) ->
        let block = map_block m (Block.Footnote.block fn) in
        Block.Footnote.Def ({ fn with block }, meta)
    | def -> def
    in
    let block = map_block m (Doc.block d) in
    let defs = Label.Map.map (map_def m) (Doc.defs d) in
    { d with Doc.block; defs }
end

module Folder = struct
  type 'a result = [ `Default | `Fold of 'a ]
  let default = `Default
  let ret v = `Fold v

  type ('a, 'b) fold = 'b t -> 'b -> 'a -> 'b
  and ('a, 'b) folder = 'b t -> 'b -> 'a -> 'b result
  and 'a t =
    { inline_ext_default : (Inline.t, 'a) fold;
      block_ext_default : (Block.t, 'a) fold;
      inline : (Inline.t, 'a) folder;
      block : (Block.t, 'a) folder; }

  let none _ _ _ = `Default
  let ext_inline_none _ _ _ = invalid_arg Inline.err_unknown
  let ext_block_none _ _ _ = invalid_arg Block.err_unknown
  let make
      ?(inline_ext_default = ext_inline_none)
      ?(block_ext_default = ext_block_none)
      ?(inline = none) ?(block = none) ()
    =
    { inline_ext_default; block_ext_default; inline; block }

  let inline_folder f = f.inline
  let block_folder f = f.block
  let inline_ext_default f = f.inline_ext_default
  let block_ext_default f = f.block_ext_default

  let rec fold_inline f acc i = match f.inline f acc i with
  | `Fold acc -> acc
  | `Default ->
      let open Inline in
      match i with
      | Autolink _ | Break _ | Code_span _ | Raw_html _ | Text _
      | Ext_math_span _ -> acc
      | Image (l, _) | Link (l, _) -> fold_inline f acc l.text
      | Emphasis ({ inline }, _) -> fold_inline f acc inline
      | Strong_emphasis ({ inline }, _) -> fold_inline f acc inline
      | Inlines (is, _) -> List.fold_left (fold_inline f) acc is
      | Ext_strikethrough (inline, _) -> fold_inline f acc inline
  | ext -> f.inline_ext_default f acc ext

  let rec fold_block f acc b = match f.block f acc b with
  | `Fold acc -> acc
  | `Default ->
      let open Block in
      match b with
      | Blank_line _ | Code_block _ | Html_block _
      | Link_reference_definition _ | Thematic_break _ | Ext_math_block _ -> acc
      | Heading (h, _) -> fold_inline f acc (Block.Heading.inline h)
      | Block_quote (bq, _) -> fold_block f acc bq.block
      | Blocks (bs, _) -> List.fold_left (fold_block f) acc bs
      | List (l, _) ->
          let fold_list_item m acc (i, _) =
            fold_block m acc (Block.List_item.block i)
          in
          List.fold_left (fold_list_item f) acc l.items
      | Paragraph (p, _) -> fold_inline f acc (Block.Paragraph.inline p)
      | Ext_table (t, _) ->
          let fold_row acc ((r, _), _) = match r with
          | (`Header is | `Data is) ->
              List.fold_left (fun acc (i, _) -> fold_inline f acc i) acc is
          | `Sep _ -> acc
          in
          List.fold_left fold_row acc t.Table.rows
      | Ext_footnote_definition (fn, _) -> fold_block f acc fn.block
      | ext -> f.block_ext_default f acc ext

  let fold_doc f acc d = fold_block f acc (Doc.block d)
end

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
