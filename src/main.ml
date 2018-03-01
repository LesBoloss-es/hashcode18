
open Common

let strategies : (string * (problem -> solution)) list =
  ["I like bonuses", Naive.i_like_bonuses]

let () =
  print_endline "==========[ Les Boloss ]==========";
  let files = Sys.readdir "problems" in
  Array.sort compare files;
  Array.iter
    (fun file ->
      Format.printf "Problem %s@." file;
      let problem = problem_of_file ("problems/"^file) in
      let scores =
        List.map
          (fun (name, strategy) ->
            Format.printf "  Strategy %s@." name;
            let solution = strategy problem in
            solution_to_file ("solutions/all/"^name^"_"^file) solution;
            let score = 0 (* score problem solution *) in
            Format.printf "    Score = %d@." score;
            (score, name, solution))
          strategies
      in
      match List.sort compare scores with
      | (_, name, solution) :: _ ->
         Format.printf "  Taking %s's solution@." name;
         solution_to_file ("solutions/bests/"^file) solution
      | _ -> assert false)
    files;
  print_endline "==========[ Boloss Out ]=========="
