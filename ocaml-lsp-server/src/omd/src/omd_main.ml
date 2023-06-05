(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(** This module implements an end-user interface for OMD.  

    Treatments that are not specific to Markdown (such as table of
    contents generation) are done here. If you want to build an
    alternative end-user Markdown tool using OMD, you might want to
    fork this file or get inspiration from it.

    Happy coding!
*)

open Omd

let remove_comments l =
  let open Omd_representation in
  let rec loop = function
    | true, Exclamations n :: tl when n > 0 ->
      loop (true,
            Omd_utils.eat (function Newline|Newlines _ -> false|_-> true) tl)
    | _, (Newline|Newlines _ as e)::tl ->
      e::loop (true, tl)
    | _, e::tl ->
      e::loop (false, tl)
    | _, [] -> []
  in loop (true, l)

let remove_endline_comments l =
  let open Omd_representation in
  let rec loop = function
    | Backslash :: (Exclamations n as e) :: tl when n > 0 ->
      e :: loop tl
    | Backslashs b :: (Exclamations n as e) :: tl when n > 0 && b mod 2 = 1 ->
      Backslashs(b-1) :: e :: loop tl
    | Exclamations n :: tl when n > 0 ->
      loop (Omd_utils.eat (function Newline|Newlines _ -> false|_-> true) tl)
    | e::tl ->
      e::loop tl
    | [] -> []
  in loop l


let preprocess_functions = ref []

(** [a += b] is a shortcut for [a := b :: !a] // NON-EXPORTED *)
let (+=) a b = a := b :: !a

let preprocess l =
  List.fold_left (fun r e -> e r)
    l
    !preprocess_functions

let otoc = ref false

let toc = ref false

let omarkdown = ref false

let notags = ref false

let toc_depth = ref 2

let toc_start = ref([]: int list)

let nl2br = ref false

let protect_html_comments = ref false

let code_stylist =
  let module M = Map.Make(String) in
object
  val mutable stylists =
    M.empty
  method style ~lang code =
    try (M.find lang stylists) code
    with Not_found ->
      try (M.find "_" stylists) code
      with Not_found -> code
  method register ~lang stylist =
    stylists <- M.add lang stylist stylists
end

let code_stylist_of_program p =
  fun code ->
    let tmp1 = Filename.temp_file "code" "bef" in
    let tmp2 = Filename.temp_file "code" "aft" in
    let () = at_exit (fun () -> Sys.remove tmp1; Sys.remove tmp2) in
    let otmp1 = open_out_bin tmp1 in
    Printf.fprintf otmp1 "%s%!" code;
    close_out otmp1;
    match Sys.command (Printf.sprintf "( cat %s | %s ) > %s" tmp1 p tmp2) with
    | 0 ->
      let cat f =
        let ic = open_in f in
        let b = Buffer.create 64 in
        try
          while true do
            Buffer.add_char b (input_char ic)
          done;
          assert false
        with End_of_file -> Buffer.contents b
      in
      cat tmp2
  | _ -> code

let register_code_stylist_of_program x =
  try
    let i = String.index x '=' in
    code_stylist#register
      ~lang:(String.sub x 0 i)
      (code_stylist_of_program
         (String.sub x (i+1) (String.length x - (i+1))))
  with Not_found | Invalid_argument _ ->
    Printf.eprintf "Error: Something wrong with [-r %s]\n" x;
    exit 1

let register_default_language l =
  Omd_backend.default_language := l


(* HTML comments might contain some double-dash (--) that are not well
    treated by HTML parsers. For instance "<!-- -- -->" should be
    translated to "<!-- &#45;&#45; -->" when we want to ensure that
    the generated HTML is correct! *)
let patch_html_comments l =
  let htmlcomments s =
    let b = Buffer.create (String.length s) in
      for i = 0 to 3 do
        Buffer.add_char b s.[i]
      done;
      for i = 4 to String.length s - 4 do
        match s.[i] with
          | '-' as c ->
              if (i > 4 && s.[i-1] = '-')
                || (i < String.length s - 5 && s.[i+1] = '-')
              then
                Printf.bprintf b "&#%d;" (int_of_char c)
              else
                Buffer.add_char b c
          | c -> Buffer.add_char b c
      done;
      for i = String.length s - 3 to String.length s - 1 do
        Buffer.add_char b s.[i]
      done;
      Buffer.contents b
  in
  let rec loop accu = function
  | Html_comment s :: tl ->
      loop (Html_comment(htmlcomments s)::accu) tl
  | e :: tl ->
      loop (e :: accu) tl
  | [] -> List.rev accu
  in loop [] l


let tag_toc l =
  let open Omd_representation in
  let x =
    object(self)
      (* [shield] is used to prevent endless loops.
         If one wants to use system threads at some point,
         and calls methods of this object concurrently,
         then there is a real problem. *)
      val remove = fun e md ->
        visit
          (function X(v) when v==e-> Some[] | _ -> None)
          md
      method name = "toc"
      method to_html ?indent:_ f md =
        let r = f (Omd.toc(remove self md)) in
        Some r
      method to_sexpr f md =
        let r = f (Omd.toc(remove self md)) in
        Some r
      method to_t md =
        let r = (Omd.toc(remove self md)) in
        Some r
    end
  in
  let rec loop = function
    | Star::
      Word "Table"::Space::
      Word "of"::Space::
      Word "contents"::Star::tl ->
      Tag("tag_toc",
          object
            method parser_extension r p l =
              Some(X(x)::r,p,l)
            method to_string = ""
          end
         ) :: loop tl
    | e::tl -> e::loop tl
    | [] -> []
  in loop l



let split_comma_int_list s =
  if s = "" then []
  else (
    let l = ref [] in
    let i = ref 0 in
    try
      while true do
        let j = String.index_from s !i ',' in
        l := int_of_string(String.sub s !i (j - !i)) :: !l;
        i := j + 1
      done;
      assert false
    with Not_found ->
      l := (int_of_string(String.sub s !i (String.length s - !i))) :: !l;
      List.rev !l
  )

module E = Omd_parser.Default_env(struct end)

let omd_gh_uemph_or_bold_style =
  ref E.gh_uemph_or_bold_style
let omd_blind_html =
  ref E.blind_html
let omd_strict_html = 
  ref E.strict_html
let omd_warning = ref E.warning
let omd_warn_error = ref E.warn_error

let list_html_tags ~inline =
  let module Parser = Omd_parser.Make(E)
  in
  if inline then
    Omd_utils.StringSet.iter
      (fun e -> print_string e; print_char '\n')
      Parser.inline_htmltags_set
  else
    Omd_utils.StringSet.iter
      (fun e -> print_string e; print_char '\n')
      Parser.htmltags_set

let verbatim_start = ref ""
let verbatim_end = ref ""
let lex_with_verb_extension s =
  if !verbatim_start = "" || !verbatim_end = "" then
    Omd_lexer.lex s
  else
    begin
      let module M = struct
        type t = Verb of string | To_lex of string
      end in
      let open M in
      let sl = String.length s
      and stl = String.length !verbatim_start
      and enl = String.length !verbatim_end in
      let rec seek_start accu from i =
        if i + stl + enl > sl then
          To_lex(String.sub s from (sl - from))::accu
        else if String.sub s i stl = !verbatim_start then
          seek_end
            (To_lex(String.sub s from (i - from))::accu)
            (i+stl)
            (i+stl)
        else seek_start accu from (i+1)
      and seek_end accu from i =
        if i + enl > sl then
          To_lex(String.sub s from (sl - from))::accu
        else if String.sub s i enl = !verbatim_end then
          seek_start
            (Verb(String.sub s from (i - from))::accu)
            (i+enl)
            (i+enl)
        else seek_end accu from (i+1)
      in
      let first_pass () = seek_start [] 0 0 in
      let second_pass l =
        List.rev_map
          (function
            | To_lex x ->
              Omd_lexer.lex x 
            | Verb x ->
              [Omd_representation.Tag(
                  "raw",
                  object
                    method parser_extension r p l =
                      match p with
                      | [] | [Omd_representation.Newlines _] ->
                        Some(Raw_block x :: r, [Omd_representation.Space], l)
                      | _ ->
                        Some(Raw x :: r, [Omd_representation.Space], l)
                    method to_string = x
                  end
                )]
          )
          l        
      in
      List.flatten(second_pass(first_pass()))
    end


let main () =
  let input = ref []
  and output = ref ""
  in
  Arg.(
    parse
      (align[
        "-o", Set_string output,
        "file.html Specify the output file (default is stdout).";
        "--", Rest(fun s -> input := s :: !input),
        " Consider all remaining arguments as input file names.";
        "-u", Clear(omd_gh_uemph_or_bold_style),
        " Use standard Markdown style for emph/bold when using `_'.";
        "-c", Unit(fun () -> preprocess_functions += remove_endline_comments),
        " Ignore lines that start with `!!!' (3 or more exclamation points).";
        "-C", Unit(fun () -> preprocess_functions += remove_comments),
        " Ignore everything on a line after `!!!' \
         (3 or more exclamation points).";
        "-m", Set(omarkdown), " Output Markdown instead of HTML.";
        "-notags", Set(notags), " Output without the HTML tags.";
        "-toc", Set(toc),
        " Replace `*Table of contents*' by the table of contents.";
        "-otoc", Set(otoc), " Output only the table of contents.";
        "-ts", String(fun l -> toc_start := split_comma_int_list l),
        "f Section for the Table of contents (default: all).";
        "-td", Set_int(toc_depth), "f Table of contents depth (default is 2).";
        "-H", Set(protect_html_comments), " Protect HTML comments.";
        "-r", String(register_code_stylist_of_program),
        "l=p Register program p as a code highlighter for language l.";
        "-R", String(register_default_language),
        "l Registers unknown languages to be l instead of void.";
        "-nl2br", Set(nl2br), " Convert new lines to <br/>.";
        "-x", String(ignore),
        "ext Activate extension ext (not yet implemented).";
        "-l", Unit ignore,
        " List available extensions ext (not yet implemented).";
        "-b", Set(omd_blind_html),
        " Don't check validity of HTML tag names.";
        "-s", Set(omd_strict_html),
        " (might not work as expected yet) Block HTML only in block HTML, \
           inline HTML only in inline HTML \
           (semantics undefined if use both -b and -s).";
        "-LHTML", Unit(fun () -> list_html_tags ~inline:false; exit 0),
        " List all known HTML tags";
        "-LHTMLi", Unit(fun () -> list_html_tags ~inline:true; exit 0),
        " List all known inline HTML tags";
        "-version", Unit(fun () -> print_endline "This is version VERSION.";
                                exit 0), " Print version.";
        "-VS", Set_string(verbatim_start),
        "start Set the start token to use to declare a verbatim section. \
        If you use -VE, you must use -VS, and both must be non-empty.";
        "-VE", Set_string(verbatim_end),
        "end Set the end token to use to declare a verbatim section. \
        If you use -VE, you must use -VS, and both must be non-empty.";
        "-w", Set(omd_warning),
        " Activate warnings (beta).";
        "-W", Set(omd_warn_error),
        " Convert warnings to errors, implies -w (beta).";
      ])
      (fun s -> input := s :: !input)
      "omd [options] [inputfile1 .. inputfileN] [options]"
  );
  let input_files =
    if !input = [] then
      [stdin]
    else
      List.rev_map (open_in) !input
  in
  let output =
    if !output = "" then
      stdout
    else
      open_out_bin !output
  in
  List.iter (fun ic ->
    let b = Buffer.create 64 in
    try while true do
        Buffer.add_char b (input_char ic)
      done; assert false
    with End_of_file ->
      let lexed = lex_with_verb_extension(Buffer.contents b) in
      let preprocessed = preprocess (if !toc then tag_toc lexed else lexed) in
      let module E = Omd_parser.Default_env(struct end) in
      let module Parser = Omd_parser.Make(
        struct
          include E
          let warning = !omd_warning || !omd_warn_error
          let warn_error = !omd_warn_error
          let gh_uemph_or_bold_style = !omd_gh_uemph_or_bold_style
          let blind_html = !omd_blind_html
          let strict_html = !omd_strict_html
        end)
      in
      let parsed1 = Parser.parse preprocessed in
      let parsed2 =
        if !protect_html_comments then
          patch_html_comments parsed1
        else
          parsed1
      in
      let parsed = parsed2 in
      let o1 = (* make either TOC or paragraphs, or leave as it is *)
        (if !otoc then Omd.toc ~start:!toc_start ~depth:!toc_depth
         else Parser.make_paragraphs)
          parsed in
      let o2 = (* output either Text or HTML, or markdown *)
        if !notags then to_text o1
        else if !omarkdown then to_markdown o1
        else if !toc && not !otoc then
          to_html
            ~pindent:true ~nl2br:false ~cs:code_stylist#style
            (* FIXME: this is a quick fix for -toc which doesn't work
               if to_html is directly applied to o1, and that seems to have 
               something to do with Parser.make_paragraphs, which seems to 
               prevent tag_toc from working properly when using to_html!
            *)
            (Parser.make_paragraphs(Parser.parse(Omd_lexer.lex(to_markdown o1))))
        else
          to_html
            ~pindent:true ~nl2br:false ~cs:code_stylist#style
            (* The normal behaviour is to convert directly, like this. *)
            o1
      in
        output_string output o2;
        if o2 <> "" && o2.[String.length o2 - 1] <> '\n' then
          output_char output '\n';
        flush output;
        if false && Omd_utils.debug then
          print_endline
            (Omd_backend.sexpr_of_md
               (Omd_parser.default_parse
                  (preprocess(Omd_lexer.lex (Buffer.contents b)))));
  )
    input_files


(* call the main function *)
let () =
  try
    main ()
  with
  | Omd_utils.Error msg when not Omd_utils.debug ->
    Printf.eprintf "(OMD) Error: %s\n" msg;
    exit 1
  | Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
