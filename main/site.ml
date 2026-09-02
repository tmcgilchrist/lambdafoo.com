(* The lambdafoo.com generator.

   Run from the repository root: every path here is relative to the working
   directory.
     dune exec main/site.exe -- --help     (the command list)
     dune exec main/site.exe -- build      (build into _site) *)

open Yocaml

(* ------------------------------------------------------------------ *)
(* Site configuration constants.                                      *)

let root_url = "https://lambdafoo.com"
let site_title = "Perpetually Curious"
let author_name = "Tim McGilchrist"
let author_email = "timmcgil@gmail.com"
let feed_title = "Perpetually Curious Blog"

let feed_description =
  "Personal opinions on technology, functional programming and various systems \
   topics."

(* Dev mode. Set from the command line before any action runs, since [Target]
   and the [dev] template flag both read it. Drafts render into _site_dev, so
   they can never reach _site. *)
let include_drafts = ref false

(* Everything the generator reads, relative to the repository root. *)
module Source = struct
  let posts = Path.rel [ "posts" ]
  let drafts = Path.rel [ "drafts" ]
  let pages = Path.rel [ "pages" ]
  let css = Path.rel [ "css" ]
  let images = Path.rel [ "images" ]
  let talks = Path.rel [ "talks" ]
  let index = Path.rel [ "index.html" ]
  (* The GitHub Pages custom domain, copied verbatim to the site root. The
     deploy runs with clean:true, so an absent CNAME unsets the domain. *)
  let cname = Path.rel [ "CNAME" ]
  let templates = Path.rel [ "templates" ]

  (* Extra TextMate grammars, loaded by filename. See grammars/README.md. *)
  let grammars = Path.rel [ "main"; "grammars" ]

  (* Third-party grammars *)
  let vendored_grammars = Path.(grammars / "vendor")
  let template file = Path.(templates / file)
  (* Tracked so a change to the generator rebuilds every page. Must be
     [Sys.executable_name], not [Sys.argv.(0)], which does not resolve under
     `dune exec`. *)
  let binary = Path.from_string Sys.executable_name
end

(* Functions, not constants: the output directory is not known until the
   command line has been parsed. *)
module Target = struct
  let base () = Path.rel [ (if !include_drafts then "_site_dev" else "_site") ]
  (* Deliberately outside [base ()]: everything under the output directory is
     published, and the build cache is not part of the site. Dev and release
     builds keep separate caches. *)
  let cache () =
    Path.rel [ "_cache"; (if !include_drafts then "site_dev" else "site") ]
  let posts () = Path.(base () / "posts")
  let drafts () = Path.(base () / "drafts")
  let drafts_index () = Path.(base () / "drafts.html")
  let pages () = Path.(base () / "pages")
  let atom () = Path.(base () / "atom.xml")
  let rss () = Path.(base () / "rss.xml")
  let archive () = Path.(base () / "archive.html")
  let sitemap () = Path.(base () / "sitemap.xml")
  let index () = Path.(base () / "index.html")
  let tags () = Path.(base () / "tags")
  let css () = Path.(base () / "css")
  let images () = Path.(base () / "images")
  let talks () = Path.(base () / "talks")

  let as_html into file =
    file |> Path.move ~into |> Path.change_extension "html"
end

(* Public URL of a post or page: /posts/<basename>.html. *)
let url_of_post file =
  Path.to_string (Target.as_html (Path.abs [ "posts" ]) file)

let url_of_page file =
  Path.to_string (Target.as_html (Path.abs [ "pages" ]) file)

let url_of_draft file =
  Path.to_string (Target.as_html (Path.abs [ "drafts" ]) file)

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

(* Every post carries a `date:` equal to its filename prefix, so the field is
   read directly. The filename fallback below is for drafts. *)
let is_digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

(* Title fallback for a draft: "2024-11-04-dwarf-part-1.md" becomes
   "Dwarf part 1". *)
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
(* Metadata                                                           *)

let common_fields =
  Data.
    [
      ("site_title", string site_title);
      (* Lets the layout show dev-only chrome. False in a deployed build. *)
      ("dev", bool !include_drafts);
      ("root", string root_url);
      ("baseurl", string "");
    ]

(* A post in posts/. Drafts reuse this type through a lenient validator. *)
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

  let tags_validator = Data.Validation.(list_of string)

  (* For a draft with no date anywhere. Templates key off [has_date]. *)
  let undated =
    Archetype.Datetime.make ~year:1970 ~month:1 ~day:1 () |> Result.get_ok

  (* Front matter first, then the filename, then the sentinel. *)
  let first_date candidates =
    Option.value ~default:undated (List.find_opt Option.is_some candidates
                                   |> Option.join)

  let validate ~url ~fallback_date data =
    let open Data.Validation in
    record
      (fun fields ->
        let+ title = required fields "title" string
        and+ date = optional fields "date" Archetype.Datetime.validate
        and+ tags = optional_or fields ~default:[] "tags" tags_validator
        and+ description = optional fields "description" string
        and+ author = optional fields "author" string in
        let date = first_date [ date; fallback_date ] in
        { title; date; tags; description; author; url })
      data

  (* Drafts are work in progress, so nothing is required. *)
  let validate_draft ~url ~fallback_title ~fallback_date data =
    let open Data.Validation in
    record
      (fun fields ->
        let+ title = optional fields "title" string
        and+ date = optional fields "date" Archetype.Datetime.validate
        and+ tags = optional_or fields ~default:[] "tags" tags_validator
        and+ description = optional fields "description" string
        and+ author = optional fields "author" string in
        let date = first_date [ date; fallback_date ] in
        {
          title = Option.value title ~default:fallback_title;
          date;
          tags;
          description;
          author;
          url;
        })
      data

  (* One per file, since [validate] needs the path for the URL and the date
     fallback. *)
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

      (* A draft with no front matter at all is fine. *)
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

(* A page in pages/. No tags, and the date is optional. *)
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
(* Reading posts                                                      *)

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
(* Markdown, with syntax highlighting                                 *)

let add_name name = function
  | `Assoc assoc -> `Assoc (("name", `String name) :: assoc)
  | j -> j

(* Registered under the filename, so `erlang.json` answers to ```erlang. `+`
   in the stem gives aliases: `yaml+yml.json` answers to both. *)
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

(* Loaded as-is, keeping the grammar's own `name` and `scopeName`. See
   grammars/vendor/README.md. *)
let load_vendored_grammar_file tm path =
  Yojson.Basic.from_file path
  |> TmLanguage.of_yojson_exn |> TmLanguage.add_grammar tm

(* Registers a grammar under a fence label that differs from its own `name`.
   See grammars/vendor/README.md. *)
let vendored_aliases_file = "aliases.json"

let load_vendored_aliases tm dir =
  let dir = Path.to_string dir in
  let path = Filename.concat dir vendored_aliases_file in
  if Sys.file_exists path then
    match Yojson.Basic.from_file path with
    | `Assoc entries ->
        List.iter
          (fun (name, file) ->
            match file with
            | `String file when not (String.starts_with ~prefix:"_" name) -> (
                let file = Filename.concat dir file in
                try
                  match Yojson.Basic.from_file file with
                  | `Assoc fields ->
                      `Assoc
                        (("name", `String name)
                        :: List.remove_assoc "name" fields)
                      |> TmLanguage.of_yojson_exn |> TmLanguage.add_grammar tm
                  | _ -> ()
                with exn ->
                  Printf.eprintf "warning: alias %s -> %s (%s)\n" name file
                    (Printexc.to_string exn))
            | _ -> ())
          entries
    | _ -> ()

let load_grammar_dir tm load dir =
  let dir = Path.to_string dir in
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f ->
           Filename.check_suffix f ".json" && f <> vendored_aliases_file)
    |> List.sort String.compare
    |> List.iter (fun f ->
        let path = Filename.concat dir f in
        try load tm path
        with exn ->
          Printf.eprintf "warning: ignoring grammar %s (%s)\n" path
            (Printexc.to_string exn))

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
  load_vendored_aliases t Source.vendored_grammars;
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
(* Actions                                                            *)

(* `images/*` and `css/*` are single-level, `talks/**/*` is recursive, and
   dotfiles are skipped. *)
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

let process_assets () =
  let open Eff in
  copy_flat ~into:(Target.css ()) Source.css
  >=> copy_flat ~into:(Target.images ()) Source.images
  >=> copy_tree ~into:(Target.talks ()) Source.talks
  >=> Action.copy_file ~into:(Target.base ()) Source.cname

let process_post file =
  let open Task in
  Action.Static.write_file_with_metadata
    Target.(as_html (posts ()) file)
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

(* As a post, but through the lenient archetype and the draft template. *)
let process_draft file =
  let open Task in
  Action.Static.write_file_with_metadata
    Target.(as_html (drafts ()) file)
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

let process_drafts_index () =
  let open Task in
  Action.Static.write_file (Target.drafts_index ())
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
    Target.(as_html (pages ()) file)
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

let process_archive () =
  write_listing ~target:(Target.archive ())
    ~template:(Source.template "archive.html")
    ~title:"Archives" ~limit:None

let process_index () =
  let open Task in
  Action.Static.write_file (Target.index ())
    ((let+ posts = fetch_posts and+ body = Pipeline.read_file Source.index in
      let _, body =
        Metadata.extract_from_content ~strategy:Metadata.jekyll body
      in
      ({ Listing.title = "Home"; posts = take 10 posts }, body))
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
    Path.(Target.tags () / tag / "index.html")
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

let process_sitemap () =
  let open Task in
  Action.Static.write_file (Target.sitemap ())
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

(* A legacy URL, rendered through templates/redirect.html. No layout: a
   redirect page is never read. *)
module Redirect = struct
  type t = { target : string }

  let normalize r = Data.[ ("target", string r.target) ] @ common_fields
end

let process_redirects =
  Action.batch_list Redirects.table (fun (from, into) ->
      (* [from] carries slashes, so split it into real path fragments. *)
      let path =
        String.split_on_char '/' from
        |> List.fold_left (fun acc frag -> Path.(acc / frag)) (Target.base ())
      in
      Action.Static.write_file path
        Task.(
          Pipeline.track_files
            [ Source.binary; Source.template "redirect.html" ]
          >>| (fun () -> ({ Redirect.target = into }, ""))
          >>> Yocaml_jingoo.Pipeline.as_template
                (module Redirect)
                (Source.template "redirect.html")
          >>| snd))

(* Feeds *)

let feed_author = Yocaml_syndication.Person.make ~email:author_email author_name

(* The feed carries each whole article, rendered through post.html. *)
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
  (* The guid is what a reader keys on to decide an item is not new. Inferring
     it from the link matches the URL the previous generator emitted, so
     existing subscribers see no duplicates. *)
  Rss2.item ~title:p.title ~link:(root_url ^ p.url) ~description:body
    ~guid:Rss2.guid_from_link ~pub_date:(Datetime.make p.date) ()

let process_atom () =
  let open Task in
  Action.Static.write_file (Target.atom ())
    (fetch_feed_entries
    >>> Yocaml_syndication.Atom.from
          ~title:(Yocaml_syndication.Atom.text feed_title)
          ~subtitle:(Yocaml_syndication.Atom.text feed_description)
          ~updated:(Yocaml_syndication.Atom.updated_from_entries ())
          ~authors:Nel.(singleton feed_author)
          ~id:(root_url ^ "/atom.xml")
          ~links:[ Yocaml_syndication.Atom.self (root_url ^ "/atom.xml") ]
          atom_entry)

let process_rss () =
  let open Task in
  Action.Static.write_file (Target.rss ())
    (fetch_feed_entries
    >>> Yocaml_syndication.Rss2.from ~title:feed_title ~site_url:root_url
          ~feed_url:(root_url ^ "/rss.xml") ~description:feed_description
          rss_item)

(* ------------------------------------------------------------------ *)
(* Driver                                                             *)

(* One page per distinct tag, so the tag set must be known before the actions
   are built. *)
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
  Action.restore_cache (Target.cache ())
  >>= process_assets () >>= process_posts >>= process_pages
  >>= Action.batch_list tags process_tag
  >>= process_index () >>= process_archive () >>= process_sitemap ()
  >>= process_atom () >>= process_rss () >>= process_redirects
  >>= (if !include_drafts then process_drafts >=> process_drafts_index ()
       else Eff.return)
  >>= Action.store_cache (Target.cache ())

(* ------------------------------------------------------------------ *)
(* Draft tooling                                                      *)

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

(* The block between the first two `---` lines, or None if there is none. *)
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

(* [has_field] is key present, [field] is key present with a value. A draft may
   leave `date:` and `tags:` empty, so the two are not the same question. *)
let has_field name lines =
  let prefix = name ^ ":" in
  let n = String.length prefix in
  List.exists
    (fun line -> String.length line >= n && String.sub line 0 n = prefix)
    lines

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

(* Reports which drafts are shaped correctly. A draft carries `title:`, `date:`
   and `tags:`, of which only the title needs a value, and its filename has no
   date prefix: the prefix is added when it moves to posts/. Returns false if a
   named draft does not exist. *)
let check_drafts only =
  let all = markdown_files Source.drafts in
  let unknown = List.filter (fun n -> not (List.mem n all)) only in
  List.iter
    (fun n -> Printf.eprintf "check-drafts: no such draft: %s\n" n)
    unknown;
  let files =
    match only with
    | [] -> all
    | names -> List.filter (fun f -> List.mem f names) all
  in
  let plural n singular = if n = 1 then singular else singular ^ "s" in
  let blocked = ref 0 and untagged = ref 0 and odd_date = ref 0 in
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
        if not (has_field "date" fm) then block "no date: field";
        if not (has_field "tags" fm) then block "no tags: field";
        (match field "date" fm with
        | Some d when Result.is_error (Archetype.Datetime.validate (Data.string d))
          ->
            incr odd_date;
            notes := "date: does not parse" :: !notes
        | Some _ -> ()
        | None -> notes := "date not set" :: !notes);
        if field "tags" fm = None then (
          incr untagged;
          notes := "tags not set" :: !notes));
    if date_from_filename (Path.rel [ file ]) <> None then
      block "filename has a date prefix, drop it until it moves to posts/";
    let note =
      match !notes with [] -> "" | ns -> "(" ^ String.concat ", " ns ^ ")"
    in
    match List.rev !blockers with
    | [] -> Printf.printf "  ready    %-52s %s\n" file note
    | bs ->
        incr blocked;
        Printf.printf "  blocked  %-52s %s\n" file (String.concat "; " bs)
  in
  let n = List.length files in
  Printf.printf "%d %s in %s\n\n" n (plural n "draft")
    (Path.to_string Source.drafts);
  List.iter report files;
  Printf.printf "\n%d well formed, %d need work\n" (n - !blocked) !blocked;
  if !untagged > 0 then
    Printf.printf
      "%d %s no tags yet, which is fine for a draft but means no tag page \
       once published\n"
      !untagged
      (if !untagged = 1 then "has" else "have");
  if !odd_date > 0 then
    Printf.printf "%d %s a `date:` that does not parse\n" !odd_date
      (if !odd_date = 1 then "has" else "have");
  unknown = []

(* Moves a draft into posts/: adds the YYYY-MM-DD prefix to the filename and
   normalises `date:` to match. Refuses anything that would not be a valid post,
   so the fields a draft may leave empty have to be filled in first. The draft
   is only removed once the post has been written. *)
let today () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

let publish_draft ~date file =
  let dir = Path.to_string Source.drafts in
  let src = Filename.concat dir file in
  let fail fmt = Printf.ksprintf (fun m -> Printf.eprintf "publish: %s\n" m; false) fmt in
  if not (Sys.file_exists src) then fail "no such draft: %s" file
  else
    let lines = read_lines src in
    match front_matter lines with
    | None -> fail "%s has no front matter" file
    | Some fm -> (
        (* A post needs all three. A draft may leave date and tags empty, so
           this is where that has to be made good. *)
        let missing =
          List.filter
            (fun k -> field k fm = None)
            [ "title"; "tags" ]
        in
        let chosen =
          match date with
          | Some d -> Some d
          | None -> (
              match field "date" fm with
              | Some d when String.length d >= 10 -> Some (String.sub d 0 10)
              | _ -> Some (today ()))
        in
        let bad_date =
          match chosen with
          | Some d ->
              Result.is_error (Archetype.Datetime.validate (Data.string d))
          | None -> true
        in
        match (missing, chosen, bad_date) with
        | _ :: _, _, _ ->
            fail "%s needs a value for: %s" file (String.concat ", " missing)
        | _, Some d, true -> fail "%s is not a YYYY-MM-DD date" d
        | _, None, _ -> fail "no date"
        | [], Some date, false ->
            let stem = Filename.remove_extension file in
            let ext = Filename.extension file in
            let dst =
              Filename.concat
                (Path.to_string Source.posts)
                (Printf.sprintf "%s-%s%s" date stem ext)
            in
            if Sys.file_exists dst then fail "%s already exists" dst
            else begin
              let fm =
                List.map
                  (fun l ->
                    if String.length l >= 5 && String.sub l 0 5 = "date:" then
                      "date: " ^ date
                    else l)
                  fm
              in
              let body =
                let rec drop n = function
                  | l :: tl when n > 0 || String.trim l <> "---" ->
                      drop (n - 1) tl
                  | rest -> rest
                in
                drop 1 lines
              in
              let oc = open_out dst in
              output_string oc (String.concat "\n" (("---" :: fm) @ body));
              close_out oc;
              (* Only now is it safe to drop the draft. *)
              if Sys.file_exists dst then Sys.remove src;
              Printf.printf "%s\n  -> %s\n" src dst;
              true
            end)

let slug_of_title title =
  let buf = Buffer.create (String.length title) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
        Buffer.add_char buf c
      else if c >= 'A' && c <= 'Z' then
        Buffer.add_char buf (Char.lowercase_ascii c)
      else if
        Buffer.length buf > 0 && Buffer.nth buf (Buffer.length buf - 1) <> '-'
      then Buffer.add_char buf '-')
    title;
  let s = Buffer.contents buf in
  let n = String.length s in
  if n > 0 && s.[n - 1] = '-' then String.sub s 0 (n - 1) else s

(* Scaffolds drafts/slug.md. The date is left empty and the filename carries no
   prefix: both are settled when the draft moves to posts/. *)
let new_draft title =
  let slug = slug_of_title title in
  if slug = "" then (
    prerr_endline
      "new-draft: give a title, e.g. dune exec main/site.exe -- new-draft \
       \"On DWARF\"";
    exit 1);
  let path = Filename.concat (Path.to_string Source.drafts) (slug ^ ".md") in
  if Sys.file_exists path then (
    Printf.eprintf "new-draft: %s already exists\n" path;
    exit 1);
  if not (Sys.file_exists (Path.to_string Source.drafts)) then
    Sys.mkdir (Path.to_string Source.drafts) 0o755;
  let oc = open_out path in
  Printf.fprintf oc "---\ntitle: \"%s\"\ndate:\ntags:\ndescription:\n---\n\n"
    title;
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
  (* Highlights a snippet rather than just looking the grammar up, so one
     that loads but tokenises nothing counts as missing. *)
  let works lang =
    match Hilite.src_code_to_html ~tm:grammars ~lang "x y\n" with
    | Ok html -> String.length html > 0 && String.index_opt html '\'' <> None
    | Error _ -> false
    | exception _ -> false
  in
  let have, missing = List.partition (fun (lang, _) -> works lang) rows in
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 in
  Printf.printf "highlighted (%d blocks):\n" (total have);
  List.iter (fun (l, n) -> Printf.printf "  %-14s %3d\n" l n) have;
  Printf.printf "\nno grammar (%d blocks) - add main/grammars/<lang>.json:\n"
    (total missing);
  List.iter (fun (l, n) -> Printf.printf "  %-14s %3d\n" l n) missing

(* ------------------------------------------------------------------ *)
(* Command Line Interface                                             *)
open Cmdliner

(* Parsing the flag applies it. Every command that builds takes this term. *)
let drafts_flag =
  let doc =
    "Include drafts/ in the build. Output moves to _site_dev/ so that a draft \
     can never reach the deployed _site/."
  in
  let apply drafts = include_drafts := drafts in
  Term.(const apply $ Arg.(value & flag & info [ "drafts" ] ~doc))

let build_cmd =
  let doc = "Build the site" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Writes the complete site into _site/, or into _site_dev/ with \
         --drafts. Rebuilds are content-hashed, so editing one post rewrites \
         one file. Delete the output directory for a full rebuild.";
    ]
  in
  let run () = Yocaml_unix.run ~level:`Info process_all in
  Cmd.v (Cmd.info "build" ~doc ~man) Term.(const run $ drafts_flag)

let serve_cmd =
  let doc = "Build the site and serve it over HTTP" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Serves the built site and rebuilds on each request, so a refresh in \
         the browser picks up an edit. Combine with --drafts while writing.";
    ]
  in
  let port =
    let doc = "Port to listen on." in
    Arg.(value & pos 0 int 8000 & info [] ~docv:"PORT" ~doc)
  in
  let run () port =
    Yocaml_unix.serve ~level:`Info ~target:(Target.base ()) ~port process_all
  in
  Cmd.v (Cmd.info "serve" ~doc ~man) Term.(const run $ drafts_flag $ port)

(* Completes draft filenames. Subcommand and option names complete for
   free. *)
let draft_conv =
  let completion =
    let complete _ctx ~token =
      Ok
        (markdown_files Source.drafts
        |> List.filter (fun f -> String.starts_with ~prefix:token f)
        |> List.map (fun f -> Arg.Completion.string f))
    in
    Arg.Completion.make complete
  in
  Arg.Conv.of_conv Arg.string ~completion ~docv:"DRAFT"

let check_drafts_cmd =
  let doc = "Report which drafts are ready to publish" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Checks every file in drafts/ against what posts/ requires. A draft is \
         blocked by missing front matter, a missing title:, or a filename with \
         no YYYY-MM-DD prefix, since the filename date is what a post \
         publishes under. Missing tags are reported but do not bplock.";
      `P "With no argument, checks every draft.";
    ]
  in
  let only =
    let doc =
      "Check only these drafts, by filename, rather than all of them."
    in
    Arg.(value & pos_all draft_conv [] & info [] ~docv:"DRAFT" ~doc)
  in
  let run only = if not (check_drafts only) then exit Cmd.Exit.cli_error in
  Cmd.v (Cmd.info "check-drafts" ~doc ~man) Term.(const run $ only)

let publish_cmd =
  let doc = "Move a draft into posts/" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Renames the draft to posts/YYYY-MM-DD-<slug>, sets its `date:` to \
         match, and removes it from drafts/. The date comes from --date, or \
         from the draft's own `date:`, or from today, in that order.";
      `P
        "Refuses a draft that would not be a valid post, so `title:` and \
         `tags:` must have values first. Nothing is removed unless the post \
         was written.";
    ]
  in
  let date =
    let doc = "Publication date, YYYY-MM-DD. Defaults to the draft's own." in
    Arg.(value & opt (some string) None & info [ "date" ] ~docv:"DATE" ~doc)
  in
  let draft =
    let doc = "Draft to publish, by filename." in
    Arg.(required & pos 0 (some draft_conv) None & info [] ~docv:"DRAFT" ~doc)
  in
  let run date draft =
    if not (publish_draft ~date draft) then exit Cmd.Exit.some_error
  in
  Cmd.v (Cmd.info "publish" ~doc ~man) Term.(const run $ date $ draft)

let new_draft_cmd =
  let doc = "Scaffold a new draft" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Creates drafts/YYYY-MM-DD-slug.md, dated today, with front matter \
         that passes check-drafts.";
      `S Manpage.s_examples;
      `Pre "site new-draft \"On DWARF and OCaml\"";
    ]
  in
  let title =
    let doc = "Title of the draft. The slug and filename come from it." in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"TITLE" ~doc)
  in
  let run words = new_draft (String.concat " " words) in
  Cmd.v (Cmd.info "new-draft" ~doc ~man) Term.(const run $ title)

let grammars_cmd =
  let doc = "Report syntax highlighting coverage" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Lists every fenced-code language used in posts/, split into those a \
         TextMate grammar is loaded for and those still unhighlighted, with \
         block counts. See main/grammars/README.md to add one.";
    ]
  in
  Cmd.v (Cmd.info "grammars" ~doc ~man) Term.(const report_grammars $ const ())

let main_cmd =
  let doc = "The lambdafoo.com static site generator" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Generates lambdafoo.com from posts/, pages/, templates/ and the \
         assets beside them.";
      `P
        "Every command reads those directories relative to the working \
         directory, so run this from the repository root.";
      `S Manpage.s_bugs;
      `P "Report issues at https://github.com/tmcgilchrist/lambdafoo.com.";
    ]
  in
  let default = Term.(ret (const (`Help (`Plain, None)))) in
  Cmd.group
    (Cmd.info "site" ~doc ~man)
    ~default
    [ build_cmd; serve_cmd; check_drafts_cmd; new_draft_cmd; publish_cmd;
      grammars_cmd ]

let () = exit (Cmd.eval main_cmd)
