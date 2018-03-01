
open Common

let strategies : (string * (problem -> solution)) list =
  [ ]
   
let () =
  print_endline "==========[ Les Boloss ]==========";
  Array.iter
    (fun file ->
      Format.printf "Problem %s@." file;
      let _problem = problem_of_file ("problems/"^file) in
      List.iter
        (fun (name, _strategy) ->
          Format.printf "  Strategy %s@." name
        (* FIXME *))
        strategies)
    (Sys.readdir "problems")
