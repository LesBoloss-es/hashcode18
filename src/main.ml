
open Common

let strategies : (string * (problem -> solution)) list =
  [ ]
   
let () =
  print_endline "==========[ Les Boloss ]==========";
  Array.iter
    (fun file ->
      Format.printf "Problem %s@." file;
      let problem = problem_of_file ("problems/"^file) in
      let scores =
        List.map
          (fun (name, strategy) ->
            Format.printf "  Strategy %s@." name;
            let solution = strategy problem in
            let score = score problem solution in
            Format.printf "    Score = %d@." score;
            (score, name, solution))
          strategies
      in
      match List.sort compare scores with
      | (_, name, solution) :: _ ->
         Format.printf "  Taking %s's solution@." name;
         solution_to_file ("solutions/"^file) solution
      | _ -> assert false)
    (Sys.readdir "problems")
