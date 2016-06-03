(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

type action =
| ActionNone
| ActionList
| ActionInit
| ActionSave
| ActionLoadDir of string
| ActionLoadFile of string

let action = ref ActionNone
let exit_status = ref 0
let output_text = ref None
let default_dir = "."
let print_only_new = ref false
let no_db = ref false

let set_action new_action =
   if !action <> ActionNone then
     raise @@ Arg.Bad
       "Options --path or --list-warnings cannot be used together";
   action := new_action

let usage_msg =
  let name = Filename.basename Sys.argv.(0) in
  String.concat "\n" [
    "Usage:";
    Printf.sprintf "   %s [OPTIONS] --path DIR" name;
    "";
  ]

let specs : (Arg.key * Arg.spec * Arg.doc) list ref = ref []

let add_spec ((cmd, _, _) as spec) =
  if List.for_all (fun (key, _, _) -> cmd <> key) !specs then
    specs := spec :: !specs;
  specs := Arg.align !specs

let () =
  specs := [
    "--init", Arg.Unit (fun dir -> set_action ActionInit),
    " Initialise a project";

    "--path", Arg.String (fun dir ->
        Lint_actions.init_config dir;
        set_action (ActionLoadDir dir)),
    "DIR   Give a project dir path";

    "--file", Arg.String (fun file ->
        set_action (ActionLoadFile file)),
    "FILE   Give a file to lint";

    "--list", Arg.Unit (fun () -> set_action ActionList),
    " List of every plugins and linters.";

    "--warn-error", Arg.Unit (fun () -> exit_status := 1),
    " Every warning returns an error status code.";

    "--save-config", Arg.Unit (fun () -> set_action (ActionSave)),
    " Save a .ocplint current configuration file.";

    "--load-plugins", Arg.String (fun files ->
        let l = (Str.split (Str.regexp ",") files) in
        Lint_actions.load_plugins l;
        List.iter add_spec (Lint_globals.Config.simple_args ())),
    "PLUGINS Load dynamically plugins with their corresponding 'cmxs' files.";

    "--no-db-cache", Arg.Set no_db,
    " Ignore the database.";

    (* Handle options to output results. *)
    "--output-txt", Arg.String (fun file -> output_text := Some file),
    "FILE   Output results in a text file.";

    (* Just during the dev *)
    "--print-only-new", Arg.Unit (fun () -> print_only_new := true),
    " Print only new warnings.";
  ]

let start_lint_file file =
  Lint_actions.init_db !no_db file;
  Lint_actions.lint_file file
  (* TODO: cago: save_db file ....*)

let start_lint dir =
  Lint_actions.init_db !no_db dir;
  Lint_actions.lint_parallel dir;
  if not !no_db then Lint_db.DefaultDB.save ();
  if Lint_db.DefaultDB.has_warning () then exit !exit_status

let main () =
  (* Getting all options declared in all registered plugins. *)
  Lint_actions.init_config default_dir;
  List.iter add_spec (Lint_globals.Config.simple_args ());
  Arg.parse_dynamic specs
    (fun cmd ->
       Printf.printf "Error: don't know what to do with %s\n%!" cmd;
       exit 1)
    usage_msg;

  match !action with
  | ActionLoadDir dir ->
    start_lint dir;
    exit 0 (* No warning, we can exit successfully *)
  | ActionLoadFile file ->
    start_lint_file file;
    exit 0
  | ActionList ->
    Lint_actions.list_plugins Format.std_formatter;
    exit 0
  | ActionInit ->
    Lint_globals.Config.save ();
    Lint_actions.init_olint_dir ()
  | ActionSave ->
    Lint_globals.Config.save ();
    exit 0
  | ActionNone ->
    start_lint default_dir;
    exit 0
