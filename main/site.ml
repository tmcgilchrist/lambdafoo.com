(* The lambdafoo.com generator, built on YOCaml 3. Ported from the Hakyll
   site.hs it replaces, so the comments below explain where Hakyll behaviour is
   being reproduced deliberately.

   Run from the repository root, since every path here is relative to the
   working directory:
     _build/default/main/site.exe          (build into _site)
     _build/default/main/site.exe serve    (build and serve on :8000)
     _build/default/main/site.exe grammars (report highlighting coverage) *)

open Yocaml

(* ------------------------------------------------------------------ *)
(* Site configuration, mirroring the constants in site.hs.             *)

let root_url = "https://lambdafoo.com"
let site_title = "Perpetually Curious"
let author_name = "Tim McGilchrist"
let author_email = "timmcgil@gmail.com"
let feed_title = "Perpetually Curious Blog"

let feed_description =
  "Personal opinions on technology, functional programming and various systems \
   topics."

(* Dev mode, for working on drafts/. Read from argv here rather than threaded
   through, because [Target] below is a module of constants and every path in
   the site hangs off it.

   Drafts render into a separate tree, so nothing in drafts/ can reach _site/
   even if you forget which mode you last built in. CI passes no flag. *)
let include_drafts = Array.exists (fun a -> a = "--drafts") Sys.argv

module Source = struct
  let posts = Path.rel [ "posts" ]
  let drafts = Path.rel [ "drafts" ]
  let pages = Path.rel [ "pages" ]
  let css = Path.rel [ "css" ]
  let images = Path.rel [ "images" ]
  let talks = Path.rel [ "talks" ]
  let index = Path.rel [ "index.html" ]
  let templates = Path.rel [ "templates" ]

  (* Extra TextMate grammars, loaded by filename. See grammars/README.md. *)
  let grammars = Path.rel [ "main"; "grammars" ]

  (* Third-party grammars, kept byte-identical to upstream. *)
  let vendored_grammars = Path.(grammars / "vendor")
  let template file = Path.(templates / file)
  let binary = Path.rel [ Sys.argv.(0) ]
end

module Target = struct
  (* NB: not named [root]. Inside [Path.(...)] that would be shadowed by
     [Path.root], the absolute "/". *)
  let base = Path.rel [ (if include_drafts then "_site_dev" else "_site") ]
  let cache = Path.(base / ".cache")
  let posts = Path.(base / "posts")
  let drafts = Path.(base / "drafts")
  let drafts_index = Path.(base / "drafts.html")
  let pages = Path.(base / "pages")
  let atom = Path.(base / "atom.xml")
  let rss = Path.(base / "rss.xml")
  let archive = Path.(base / "archive.html")
  let sitemap = Path.(base / "sitemap.xml")
  let index = Path.(base / "index.html")
  let tags = Path.(base / "tags")
  let css = Path.(base / "css")
  let images = Path.(base / "images")
  let talks = Path.(base / "talks")

  let as_html into file =
    file |> Path.move ~into |> Path.change_extension "html"
end

(* Hakyll's route for posts/pages is `setExtension "html"`, so the public URL
   is /posts/<basename>.html. *)
let url_of_post file =
  Path.to_string (Target.as_html (Path.abs [ "posts" ]) file)

let url_of_page file =
  Path.to_string (Target.as_html (Path.abs [ "pages" ]) file)

let url_of_draft file =
  Path.to_string (Target.as_html (Path.abs [ "drafts" ]) file)

(* ------------------------------------------------------------------ *)
(* Dates.                                                              *)

let month_name = function
  | Archetype.Datetime.Jan -> "January"
  | Feb -> "February"
  | Mar -> "March"
  | Apr -> "April"
  | May -> "May"
  | Jun -> "June"
  | Jul -> "July"
  | Aug -> "August"
  | Sep -> "September"
  | Oct -> "October"
  | Nov -> "November"
  | Dec -> "December"

let month_number = function
  | Archetype.Datetime.Jan -> 1
  | Feb -> 2
  | Mar -> 3
  | Apr -> 4
  | May -> 5
  | Jun -> 6
  | Jul -> 7
  | Aug -> 8
  | Sep -> 9
  | Oct -> 10
  | Nov -> 11
  | Dec -> 12

(* Hakyll renders dates with "%B %e, %Y"; %e is space padded. *)
let pretty_date (d : Archetype.Datetime.t) =
  Printf.sprintf "%s %2d, %d" (month_name d.month)
    (d.day :> int)
    (d.year :> int)

let iso_date (d : Archetype.Datetime.t) =
  Printf.sprintf "%04d-%02d-%02d"
    (d.year :> int)
    (month_number d.month)
    (d.day :> int)

(* Hakyll's getItemUTC tries the `date:` field against a fixed set of formats
   and falls back to the date in the filename when none match. Those formats
   need either a bare date or a full HH:MM:SS time, so the `YYYY-MM-DD HH:MM`
   used by 65 of the 76 posts here is rejected and the filename wins. Replicate
   that: otherwise the posts whose front matter contradicts their filename sort
   differently from the live site. *)
let is_digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let hakyll_accepts_date raw =
  match String.split_on_char ' ' (String.trim raw) with
  | [ d ] -> (
      match String.split_on_char '-' d with
      | [ y; m; dd ] -> is_digits y && is_digits m && is_digits dd
      | _ -> false)
  | [ _; time ] -> (
      (* Only a complete HH:MM:SS is accepted. *)
      match String.split_on_char ':' time with
      | [ _; _; _ ] -> true
      | _ -> false)
  | _ -> false

(* A draft need not have a `title:` yet, so fall back to its filename:
   "2024-11-04-dwarf-part-1.md" becomes "Dwarf part 1". *)
let title_from_filename file =
  let base =
    file |> Path.remove_extension |> Path.basename |> Option.value ~default:""
  in
  let words =
    match String.split_on_char '-' base with
    | y :: m :: d :: rest when is_digits y && is_digits m && is_digits d -> rest
    | words -> words
  in
  match String.concat " " words with
  | "" -> base
  | s -> String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) s

let date_from_filename file =
  let base =
    file |> Path.remove_extension |> Path.basename |> Option.value ~default:""
  in
  match String.split_on_char '-' base with
  | y :: m :: d :: _ -> (
      match (int_of_string_opt y, int_of_string_opt m, int_of_string_opt d) with
      | Some year, Some month, Some day ->
          Archetype.Datetime.make ~year ~month ~day () |> Result.to_option
      | _ -> None)
  | _ -> None

(* ------------------------------------------------------------------ *)
(* Metadata.                                                           *)

let common_fields =
  Data.
    [
      ("site_title", string site_title);
      (* Lets the layout show dev-only chrome. False in a deployed build. *)
      ("dev", bool include_drafts);
      ("root", string root_url);
      ("baseurl", string "");
    ]

module Post = struct
  type t = {
    title : string;
    date : Archetype.Datetime.t;
    tags : string list;
    description : string option;
    author : string option;
    url : string;
  }

  let entity_name = "Post"

  (* Hakyll writes `tags: ocaml, open-source`; accept that as well as a real
     YAML list, so no post front matter has to change. *)
  let split_tags s =
    String.split_on_char ',' s |> List.map String.trim
    |> List.filter (fun s -> s <> "")

  (* [/] is "try the left validator, fall back to the right one". *)
  let tags_validator =
    let open Data.Validation in
    list_of string / (string $ split_tags)

  (* Stands in for a draft that has no date anywhere. Templates key off
     [has_date] rather than printing 1970. *)
  let undated =
    Archetype.Datetime.make ~year:1970 ~month:1 ~day:1 () |> Result.get_ok

  let validate ~url ~fallback_date data =
    let open Data.Validation in
    record
      (fun fields ->
        let+ title = required fields "title" string
        and+ date = optional fields "date" string
        and+ tags = optional_or fields ~default:[] "tags" tags_validator
        and+ description = optional fields "description" string
        and+ author = optional fields "author" string in
        let from_metadata =
          Option.bind date (fun raw ->
              if hakyll_accepts_date raw then
                Archetype.Datetime.validate (Data.string raw)
                |> Result.to_option
              else None)
        in
        let date =
          match (from_metadata, fallback_date) with
          | Some d, _ -> d
          | None, Some d -> d
          | None, None ->
              (* Unreachable here: every post filename carries a date. *)
              undated
        in
        { title; date; tags; description; author; url })
      data

  (* Drafts are work in progress, so nothing may be required. Four of them
     currently have no front matter at all. Dates still go through
     [hakyll_accepts_date], so a draft previews with the date it will actually
     publish under once it moves into posts/. *)
  let validate_draft ~url ~fallback_title ~fallback_date data =
    let open Data.Validation in
    record
      (fun fields ->
        let+ title = optional fields "title" string
        and+ date = optional fields "date" string
        and+ tags = optional_or fields ~default:[] "tags" tags_validator
        and+ description = optional fields "description" string
        and+ author = optional fields "author" string in
        let from_metadata =
          Option.bind date (fun raw ->
              if hakyll_accepts_date raw then
                Archetype.Datetime.validate (Data.string raw)
                |> Result.to_option
              else None)
        in
        let date =
          match (from_metadata, fallback_date) with
          | Some d, _ -> d
          | None, Some d -> d
          | None, None -> undated
        in
        {
          title = Option.value title ~default:fallback_title;
          date;
          tags;
          description;
          author;
          url;
        })
      data

  (* [validate] needs the file path for the URL and the date fallback, which a
     plain DATA_READABLE cannot see, so build one per file. *)
  let readable_for file : (module Required.DATA_READABLE with type t = t) =
    (module struct
      type nonrec t = t

      let entity_name = entity_name
      let neutral = Error (Required.Required_metadata { entity = entity_name })

      let validate data =
        validate ~url:(url_of_post file)
          ~fallback_date:(date_from_filename file) data
    end)

  let readable_draft_for file : (module Required.DATA_READABLE with type t = t)
      =
    (module struct
      type nonrec t = t

      let entity_name = "Draft"
      let url = url_of_draft file
      let fallback_title = title_from_filename file
      let fallback_date = date_from_filename file

      (* No front matter at all is a normal state for a draft, so unlike a post
         this is a value rather than an error. *)
      let neutral =
        Ok
          {
            title = fallback_title;
            date = Option.value fallback_date ~default:undated;
            tags = [];
            description = None;
            author = None;
            url;
          }

      let validate data =
        validate_draft ~url ~fallback_title ~fallback_date data
    end)

  let normalize p =
    Data.
      [
        ("title", string p.title);
        ("url", string p.url);
        ("date", Archetype.Datetime.normalize p.date);
        ("pretty_date", string (pretty_date p.date));
        (* False only for a draft with no date anywhere. *)
        ("has_date", bool (Archetype.Datetime.compare p.date undated <> 0));
        ("isodate", string (iso_date p.date));
        ("tags", list_of string p.tags);
        ("has_tags", bool (p.tags <> []));
        ("description", option string p.description);
        ("has_description", bool (Option.is_some p.description));
        ("author", option string p.author);
      ]
    @ common_fields

  let compare_recent_first a b = ~-(Archetype.Datetime.compare a.date b.date)
end

module Page = struct
  type t = { title : string; url : string; date : Archetype.Datetime.t option }

  let entity_name = "Page"

  let readable_for file : (module Required.DATA_READABLE with type t = t) =
    (module struct
      type nonrec t = t

      let entity_name = entity_name
      let neutral = Error (Required.Required_metadata { entity = entity_name })

      let validate data =
        let open Data.Validation in
        record
          (fun fields ->
            let+ title = required fields "title" string
            and+ date = optional fields "date" Archetype.Datetime.validate in
            { title; url = url_of_page file; date })
          data
    end)

  let normalize p =
    Data.
      [
        ("title", string p.title);
        ("url", string p.url);
        ("isodate", option string (Option.map iso_date p.date));
      ]
    @ common_fields
end

(* Index, archive and per-tag listing pages all share this shape. *)
module Listing = struct
  type t = { title : string; posts : Post.t list }

  let normalize l =
    Data.
      [
        ("title", string l.title);
        ( "posts",
          list (List.map (fun p -> Data.record (Post.normalize p)) l.posts) );
        ("has_posts", bool (l.posts <> []));
      ]
    @ common_fields
end

(* The sitemap mixes posts and pages, so give it a uniform entry type. *)
module Sitemap = struct
  type entry = { url : string; isodate : string }
  type t = { posts : entry list }

  let normalize s =
    Data.
      [
        ( "posts",
          list
            (List.map
               (fun e ->
                 Data.record
                   [ ("url", string e.url); ("isodate", string e.isodate) ])
               s.posts) );
      ]
    @ common_fields
end

(* ------------------------------------------------------------------ *)
(* Reading posts.                                                      *)

let is_markdown = Path.has_extension "markdown"
let is_md = Path.has_extension "md"
let is_post file = is_markdown file || is_md file

let fetch_posts =
  let open Task in
  Pipeline.track_files [ Source.binary; Source.posts ]
  >>> Pipeline.fetch ~only:`Files ~where:is_post
        (fun file ->
          let open Eff in
          let+ metadata, _content =
            Eff.read_file_with_metadata
              (module Yocaml_yaml)
              (Post.readable_for file) ~on:`Source file
          in
          metadata)
        Source.posts
  >>| List.sort Post.compare_recent_first

let fetch_drafts =
  let open Task in
  Pipeline.track_files [ Source.binary; Source.drafts ]
  >>> Pipeline.fetch ~only:`Files ~where:is_post
        (fun file ->
          let open Eff in
          let+ metadata, _content =
            Eff.read_file_with_metadata
              (module Yocaml_yaml)
              (Post.readable_draft_for file)
              ~on:`Source file
          in
          metadata)
        Source.drafts
  >>| List.sort Post.compare_recent_first

let take n l = List.filteri (fun i _ -> i < n) l

(* ------------------------------------------------------------------ *)
(* Markdown, with syntax highlighting.                                 *)

let add_name name = function
  | `Assoc assoc -> `Assoc (("name", `String name) :: assoc)
  | j -> j

(* A hand-written grammar is registered under its filename, so `erlang.json`
   answers to ```erlang. `+` in the stem gives aliases: `yaml+yml.json` answers
   to both. This mirrors what Hilite.add_name does for its own bundled
   grammars. *)
let load_grammar_file tm path =
  let json = Yojson.Basic.from_file path in
  let names =
    Filename.basename path |> Filename.remove_extension
    |> String.split_on_char '+'
    |> List.filter (fun s -> s <> "")
  in
  List.iter
    (fun name ->
      let named =
        match json with
        | `Assoc fields ->
            `Assoc (("name", `String name) :: List.remove_assoc "name" fields)
        | other -> other
      in
      named |> TmLanguage.of_yojson_exn |> TmLanguage.add_grammar tm)
    names

(* A vendored grammar is loaded byte for byte, so it stays diffable against
   upstream. It keeps its own `name`, which the registry lowercases, and its own
   `scopeName`, which is what sibling grammars include each other by. That is
   how `c++` gets a name at all: the filename convention above cannot spell it,
   because `+` is the alias separator. *)
let load_vendored_grammar_file tm path =
  Yojson.Basic.from_file path
  |> TmLanguage.of_yojson_exn |> TmLanguage.add_grammar tm

let load_grammar_dir tm load dir =
  let dir = Path.to_string dir in
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".json")
    |> List.sort String.compare
    |> List.iter (fun f ->
        let path = Filename.concat dir f in
        try load tm path
        with exn ->
          Printf.eprintf "warning: ignoring grammar %s (%s)\n" path
            (Printexc.to_string exn))

(* YOCaml's default set omits shell, this blog's most used language. *)
let grammars =
  let t = TmLanguage.create () in
  List.iter
    (fun g -> g |> TmLanguage.of_yojson_exn |> TmLanguage.add_grammar t)
    Hilite.Grammars.
      [
        ocaml;
        ocaml_interface;
        dune;
        opam;
        diff;
        add_name "shell" shell;
        add_name "sh" shell;
        add_name "bash" shell;
      ];
  load_grammar_dir t load_vendored_grammar_file Source.vendored_grammars;
  load_grammar_dir t load_grammar_file Source.grammars;
  t

(* Unit argument keeps this polymorphic across the metadata types. *)
let markdown_to_html content =
  let highlight = Yocaml_markdown.Doc.syntax_highlighting ~tm:grammars () in
  Yocaml_markdown.from_string_to_html ~strict:false ~heading_auto_ids:true
    ~highlight content

let content_to_html () =
  Task.lift (fun (meta, content) -> (meta, markdown_to_html content))

(* ------------------------------------------------------------------ *)
(* Actions.                                                            *)

(* Hakyll skips dotfiles, and `images/*` / `css/*` are single-level globs
   whereas `talks/**/*` is recursive. Match that exactly. *)
let is_hidden path =
  match Path.basename path with
  | Some b -> String.length b > 0 && b.[0] = '.'
  | None -> false

let visible p = not (is_hidden p)

let rec copy_tree ~into source cache =
  let open Eff in
  let* entries = read_directory ~on:`Source ~only:`Both ~where:visible source in
  Stdlib.List.fold_left
    (fun acc entry ->
      let* cache = acc in
      let* file = is_file ~on:`Source entry in
      if file then Action.copy_file ~into entry cache
      else
        let name = Option.value ~default:"" (Path.basename entry) in
        copy_tree ~into:Path.(into / name) entry cache)
    (return cache) entries

let copy_flat ~into source =
  Action.batch ~only:`Files ~where:visible source (Action.copy_file ~into)

let process_assets =
  let open Eff in
  copy_flat ~into:Target.css Source.css
  >=> copy_flat ~into:Target.images Source.images
  >=> copy_tree ~into:Target.talks Source.talks

let process_post file =
  let open Task in
  Action.Static.write_file_with_metadata
    Target.(as_html posts file)
    (Pipeline.track_files
       [ Source.binary; Source.grammars; Source.vendored_grammars ]
    >>> Yocaml_yaml.Pipeline.read_file_with_metadata (Post.readable_for file)
          file
    >>> content_to_html ()
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Post)
          (Source.template "post.html")
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Post)
          (Source.template "layout.html"))

let process_posts =
  Action.batch ~only:`Files ~where:is_post Source.posts process_post

(* Same pipeline as a post, but through the lenient archetype and the draft
   template, and only ever into the dev tree. *)
let process_draft file =
  let open Task in
  Action.Static.write_file_with_metadata
    Target.(as_html drafts file)
    (Pipeline.track_files
       [ Source.binary; Source.grammars; Source.vendored_grammars ]
    >>> Yocaml_yaml.Pipeline.read_file_with_metadata
          (Post.readable_draft_for file)
          file
    >>> content_to_html ()
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Post)
          (Source.template "draft.html")
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Post)
          (Source.template "layout.html"))

let process_drafts =
  Action.batch ~only:`Files ~where:is_post Source.drafts process_draft

let process_drafts_index =
  let open Task in
  Action.Static.write_file Target.drafts_index
    (fetch_drafts
    >>| (fun drafts -> ({ Listing.title = "Drafts"; posts = drafts }, ""))
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Listing)
          (Source.template "drafts.html")
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Listing)
          (Source.template "layout.html")
    >>| snd)

let process_page file =
  let open Task in
  Action.Static.write_file_with_metadata
    Target.(as_html pages file)
    (Pipeline.track_file Source.binary
    >>> Yocaml_yaml.Pipeline.read_file_with_metadata (Page.readable_for file)
          file
    >>> content_to_html ()
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Page)
          (Source.template "page.html")
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Page)
          (Source.template "layout.html"))

let process_pages =
  Action.batch ~only:`Files ~where:is_md Source.pages process_page

(* A listing page rendered from a template plus the layout. *)
let write_listing ~target ~template ~title ~limit =
  let open Task in
  Action.Static.write_file target
    (fetch_posts
    >>| (fun posts ->
    let posts = match limit with None -> posts | Some n -> take n posts in
    ({ Listing.title; posts }, ""))
    >>> Yocaml_jingoo.Pipeline.as_template (module Listing) template
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Listing)
          (Source.template "layout.html")
    >>| snd)

let process_archive =
  write_listing ~target:Target.archive
    ~template:(Source.template "archive.html")
    ~title:"Archives" ~limit:None

(* Hakyll's index reads index.html and applies it as a template. Here the post
   list lives in the Jingoo template, so drop Hakyll's partial call. *)
let strip_hakyll_partial content =
  String.split_on_char '\n' content
  |> List.filter (fun line ->
      String.trim line <> {|$partial("templates/post-list.html")$|})
  |> String.concat "\n"

let process_index =
  let open Task in
  Action.Static.write_file Target.index
    ((let+ posts = fetch_posts and+ body = Pipeline.read_file Source.index in
      let _, body =
        Metadata.extract_from_content ~strategy:Metadata.jekyll body
      in
      ( { Listing.title = "Home"; posts = take 10 posts },
        strip_hakyll_partial body ))
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Listing)
          (Source.template "index.html")
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Listing)
          (Source.template "layout.html")
    >>| snd)

let process_tag tag =
  let open Task in
  Action.Static.write_file
    Path.(Target.tags / tag / "index.html")
    (fetch_posts
    >>| (fun posts ->
    let posts = List.filter (fun p -> List.mem tag p.Post.tags) posts in
    ({ Listing.title = Printf.sprintf "Posts tagged \"%s\"" tag; posts }, ""))
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Listing)
          (Source.template "tag.html")
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Listing)
          (Source.template "layout.html")
    >>| snd)

let process_sitemap =
  let open Task in
  Action.Static.write_file Target.sitemap
    ((let+ posts = fetch_posts
      and+ pages =
        Pipeline.fetch ~only:`Files ~where:is_md
          (fun file ->
            let open Eff in
            let+ metadata, _ =
              Eff.read_file_with_metadata
                (module Yocaml_yaml)
                (Page.readable_for file) ~on:`Source file
            in
            metadata)
          Source.pages
      in
      let post_entries =
        List.map
          (fun p ->
            { Sitemap.url = p.Post.url; isodate = iso_date p.Post.date })
          posts
      and page_entries =
        List.map
          (fun (p : Page.t) ->
            {
              Sitemap.url = p.url;
              isodate = Option.fold ~none:"" ~some:iso_date p.date;
            })
          pages
      in
      let page_entries =
        List.sort
          (fun a b -> String.compare a.Sitemap.url b.Sitemap.url)
          page_entries
      in
      ({ Sitemap.posts = post_entries @ page_entries }, ""))
    >>> Yocaml_jingoo.Pipeline.as_template
          (module Sitemap)
          (Source.template "sitemap.xml")
    >>| snd)

let redirect_page target =
  Printf.sprintf
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/><meta \
     name=\"generator\" content=\"yocaml\"/><meta name=\"viewport\" \
     content=\"width=device-width, initial-scale=1.0\"><meta \
     http-equiv=\"refresh\" content=\"0; url=%s\"><link rel=\"canonical\" \
     href=\"%s\"><title>Permanent Redirect</title></head><body><p>The page has \
     moved to: <a href=\"%s\">this page</a></p></body></html>"
    target target target

let process_redirects =
  Action.batch_list Redirects.table (fun (from, into) ->
      (* [from] carries slashes, so split it into real path fragments. *)
      let target =
        String.split_on_char '/' from
        |> List.fold_left (fun acc frag -> Path.(acc / frag)) Target.base
      in
      Action.Static.write_file target
        Task.(
          Pipeline.track_file Source.binary >>| fun () -> redirect_page into))

(* ------------------------------------------------------------------ *)
(* Feeds.                                                              *)

let feed_author = Yocaml_syndication.Person.make ~email:author_email author_name

(* Hakyll snapshots each post after templates/post.html is applied and uses
   that as the feed description, so the feed carries the whole article. *)
let fetch_feed_entries =
  let open Task in
  let+ apply_post_template =
    Yocaml_jingoo.read_template (Source.template "post.html")
  and+ entries =
    Pipeline.track_files
      [ Source.binary; Source.posts; Source.grammars; Source.vendored_grammars ]
    >>> Pipeline.fetch ~only:`Files ~where:is_post
          (fun file ->
            let open Eff in
            let+ metadata, content =
              read_file_with_metadata
                (module Yocaml_yaml)
                (Post.readable_for file) ~on:`Source file
            in
            (metadata, markdown_to_html content))
          Source.posts
  in
  entries
  |> List.sort (fun (a, _) (b, _) -> Post.compare_recent_first a b)
  |> take 10
  |> List.map (fun (m, html) ->
      (m, apply_post_template (module Post) ~metadata:m html))

let atom_entry ((p : Post.t), body) =
  let open Yocaml_syndication in
  let id = root_url ^ p.url in
  let date = Datetime.make p.date in
  Atom.entry ~title:(Atom.text p.title)
    ~links:[ Atom.alternate id ~title:p.title ]
    ~summary:(Atom.html body) ~published:date ~id ~updated:date ()

let rss_item ((p : Post.t), body) =
  let open Yocaml_syndication in
  Rss2.item ~title:p.title ~link:(root_url ^ p.url) ~description:body
    ~pub_date:(Datetime.make p.date) ()

let process_atom =
  let open Task in
  Action.Static.write_file Target.atom
    (fetch_feed_entries
    >>> Yocaml_syndication.Atom.from
          ~title:(Yocaml_syndication.Atom.text feed_title)
          ~subtitle:(Yocaml_syndication.Atom.text feed_description)
          ~updated:(Yocaml_syndication.Atom.updated_from_entries ())
          ~authors:Nel.(singleton feed_author)
          ~id:(root_url ^ "/atom.xml")
          ~links:[ Yocaml_syndication.Atom.self (root_url ^ "/atom.xml") ]
          atom_entry)

let process_rss =
  let open Task in
  Action.Static.write_file Target.rss
    (fetch_feed_entries
    >>> Yocaml_syndication.Rss2.from ~title:feed_title ~site_url:root_url
          ~feed_url:(root_url ^ "/rss.xml") ~description:feed_description
          rss_item)

(* ------------------------------------------------------------------ *)
(* Driver.                                                             *)

(* Tag pages are one file per distinct tag, so the tag set has to be known
   before the actions are built. Hakyll's buildTags does the same scan. *)
let all_tags () =
  let open Eff in
  let* files =
    read_directory ~on:`Source ~only:`Files ~where:is_post Source.posts
  in
  let rec collect acc = function
    | [] ->
        return
          (acc |> Stdlib.List.concat |> Stdlib.List.sort_uniq String.compare)
    | file :: rest ->
        let* metadata, _ =
          read_file_with_metadata
            (module Yocaml_yaml)
            (Post.readable_for file) ~on:`Source file
        in
        collect (metadata.Post.tags :: acc) rest
  in
  collect [] files

let process_all () =
  let open Eff in
  let* tags = all_tags () in
  Action.restore_cache Target.cache
  >>= process_assets >>= process_posts >>= process_pages
  >>= Action.batch_list tags process_tag
  >>= process_index >>= process_archive >>= process_sitemap >>= process_atom
  >>= process_rss >>= process_redirects
  >>= (if include_drafts then process_drafts >=> process_drafts_index
       else Eff.return)
  >>= Action.store_cache Target.cache

(* ------------------------------------------------------------------ *)
(* Draft tooling.                                                      *)

let read_lines path =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file ->
        close_in ic;
        List.rev acc
    | line -> loop (line :: acc)
  in
  loop []

(* Front matter is the block between the first two `---` lines. Returns None
   when the file has none at all, which several drafts do. *)
let front_matter lines =
  match lines with
  | first :: rest when String.trim first = "---" ->
      let rec take acc = function
        | [] -> None (* unterminated *)
        | line :: _ when String.trim line = "---" -> Some (List.rev acc)
        | line :: tl -> take (line :: acc) tl
      in
      take [] rest
  | _ -> None

(* A key with an empty value is what [new_draft] scaffolds, so treat it as
   absent rather than as filled in. *)
let field name lines =
  let prefix = name ^ ":" in
  let n = String.length prefix in
  List.find_map
    (fun line ->
      if String.length line >= n && String.sub line 0 n = prefix then
        match String.trim (String.sub line n (String.length line - n)) with
        | "" -> None
        | v -> Some v
      else None)
    lines

let markdown_files dir =
  let path = Path.to_string dir in
  if not (Sys.file_exists path && Sys.is_directory path) then []
  else
    Sys.readdir path |> Array.to_list |> List.sort String.compare
    |> List.filter (fun f ->
           Filename.check_suffix f ".md" || Filename.check_suffix f ".markdown")

(* Check every draft against what posts/ actually requires: a `title:`, and a
   date in the *filename*, because the filename is what a post publishes under.
   Everything else is advisory. In particular the `YYYY-MM-DD HH:MM` date form
   is not an error, since 65 of the 76 published posts use it too, so it is
   counted once at the end rather than reported per file. *)
let check_drafts () =
  let files = markdown_files Source.drafts in
  let blocked = ref 0 and untagged = ref 0 and inert_date = ref 0 in
  let report file =
    let path = Filename.concat (Path.to_string Source.drafts) file in
    let lines = read_lines path in
    let blockers = ref [] in
    let block s = blockers := s :: !blockers in
    let notes = ref [] in
    (match front_matter lines with
    | None -> block "no front matter"
    | Some fm ->
        if field "title" fm = None then block "no title:";
        (match field "date" fm with
        | Some d when not (hakyll_accepts_date d) -> incr inert_date
        | _ -> ());
        if field "tags" fm = None then (
          incr untagged;
          notes := "no tags" :: !notes));
    if date_from_filename (Path.rel [ file ]) = None then
      block "filename needs a YYYY-MM-DD prefix";
    let note =
      match !notes with [] -> "" | ns -> "(" ^ String.concat ", " ns ^ ")"
    in
    match List.rev !blockers with
    | [] -> Printf.printf "  ready    %-52s %s\n" file note
    | bs ->
        incr blocked;
        Printf.printf "  blocked  %-52s %s\n" file (String.concat "; " bs)
  in
  Printf.printf "%d drafts in %s\n\n" (List.length files)
    (Path.to_string Source.drafts);
  List.iter report files;
  Printf.printf "\n%d ready to publish, %d blocked\n"
    (List.length files - !blocked)
    !blocked;
  if !untagged > 0 then
    Printf.printf "%d have no tags, so they would join no tag page\n" !untagged;
  if !inert_date > 0 then
    Printf.printf
      "%d carry a `date:` in `YYYY-MM-DD HH:MM` form, which is ignored. The \
       filename\n\
      \  date is what publishes. That matches posts/, so it is not a problem.\n"
      !inert_date

let slug_of_title title =
  let buf = Buffer.create (String.length title) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
        Buffer.add_char buf c
      else if c >= 'A' && c <= 'Z' then
        Buffer.add_char buf (Char.lowercase_ascii c)
      else if Buffer.length buf > 0 && Buffer.nth buf (Buffer.length buf - 1) <> '-'
      then Buffer.add_char buf '-')
    title;
  let s = Buffer.contents buf in
  let n = String.length s in
  if n > 0 && s.[n - 1] = '-' then String.sub s 0 (n - 1) else s

(* Scaffold drafts/YYYY-MM-DD-slug.md with front matter that passes
   [check_drafts]. The date prefix is what the post will publish under, so it
   is worth getting into the filename from the start. *)
let new_draft title =
  let slug = slug_of_title title in
  if slug = "" then (
    prerr_endline "new-draft: give a title, e.g. site.exe new-draft \"On DWARF\"";
    exit 1);
  let tm = Unix.localtime (Unix.time ()) in
  let date =
    Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
  in
  let file = Printf.sprintf "%s-%s.md" date slug in
  let path = Filename.concat (Path.to_string Source.drafts) file in
  if Sys.file_exists path then (
    Printf.eprintf "new-draft: %s already exists\n" path;
    exit 1);
  if not (Sys.file_exists (Path.to_string Source.drafts)) then
    Sys.mkdir (Path.to_string Source.drafts) 0o755;
  let oc = open_out path in
  Printf.fprintf oc
    "---\ntitle: \"%s\"\ndate: %s 09:00\ntags: \ndescription: \n---\n\n" title
    date;
  close_out oc;
  Printf.printf "%s\n" path

(* Report which fenced-code languages still have no grammar. *)
let report_grammars () =
  let seen = Hashtbl.create 32 in
  let dir = Path.to_string Source.posts in
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.iter (fun f ->
      let ic = open_in (Filename.concat dir f) in
      let rec loop fenced =
        match input_line ic with
        | exception End_of_file -> close_in ic
        | line ->
            let fence =
              String.length line >= 3 && String.sub line 0 3 = "```"
            in
            (if fence && not fenced then
               let lang =
                 String.sub line 3 (String.length line - 3) |> String.trim
               in
               if lang <> "" then
                 Hashtbl.replace seen lang
                   (1 + Option.value ~default:0 (Hashtbl.find_opt seen lang)));
            loop (if fence then not fenced else fenced)
      in
      loop false);
  let rows =
    Hashtbl.fold (fun lang n acc -> (lang, n) :: acc) seen []
    |> List.sort (fun (_, a) (_, b) -> compare b a)
  in
  let have, missing =
    List.partition
      (fun (lang, _) -> Option.is_some (TmLanguage.find_by_name grammars lang))
      rows
  in
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 in
  Printf.printf "highlighted (%d blocks):\n" (total have);
  List.iter (fun (l, n) -> Printf.printf "  %-14s %3d\n" l n) have;
  Printf.printf "\nno grammar (%d blocks) - add main/grammars/<lang>.json:\n"
    (total missing);
  List.iter (fun (l, n) -> Printf.printf "  %-14s %3d\n" l n) missing

let usage () =
  prerr_endline
    "usage: site.exe [--drafts]            build into _site (_site_dev with \
     --drafts)\n\
    \       site.exe serve [--drafts] [port]\n\
    \       site.exe check-drafts          report drafts that are not ready\n\
    \       site.exe new-draft <title>     scaffold a new draft\n\
    \       site.exe grammars              syntax highlighting coverage";
  exit 1

let () =
  (* [--drafts] is a mode, not a positional argument, so strip it before
     matching. It has already been read into [include_drafts]. *)
  match List.filter (fun a -> a <> "--drafts") (Array.to_list Sys.argv) with
  | _ :: "grammars" :: _ -> report_grammars ()
  | _ :: "check-drafts" :: _ -> check_drafts ()
  | _ :: "new-draft" :: title when title <> [] ->
      new_draft (String.concat " " title)
  | _ :: "new-draft" :: _ -> usage ()
  | _ :: "serve" :: rest ->
      let port =
        Option.bind (List.nth_opt rest 0) int_of_string_opt
        |> Option.value ~default:8000
      in
      Yocaml_unix.serve ~level:`Info ~target:Target.base ~port process_all
  | _ -> Yocaml_unix.run ~level:`Info process_all
