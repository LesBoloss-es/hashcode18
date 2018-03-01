
open Common

let neg_compare x y = - compare x y

let strategies : (string * (problem -> solution)) list =
  [
    "One Bonus 1", Naive.schedule_with_score   Naive.naive_choice (Linear.one_bonus 1) ;
    "One Bonus 10", Naive.schedule_with_score  Naive.naive_choice (Linear.one_bonus 10) ;
    "One Bonus 100", Naive.schedule_with_score Naive.naive_choice (Linear.one_bonus 100) ;
    "One Bonus 1 AH", Naive.schedule_with_score   Naive.naive_choice_better (Linear.one_bonus 1) ;
    "One Bonus 10 AH", Naive.schedule_with_score  Naive.naive_choice_better (Linear.one_bonus 10) ;
    "One Bonus 100 AH", Naive.schedule_with_score Naive.naive_choice_better (Linear.one_bonus 100) ;
  ]

let () =
  print_endline "==========[ Les Boloss ]==========";
  let files = Sys.readdir "problems" in
  Array.sort compare files;
  let total_score = ref 0 in
  Array.iter
    (fun file ->
      Format.printf "Problem %s@." file;
      let problem = problem_of_file ("problems/"^file) in
      let scores =
        List.map
          (fun (name, strategy) ->
            Format.printf "  Strategy %s@." name;
            let solution =
              try
                strategy problem
              with
                _ ->
                Format.eprintf "    Strategy %s failed! Using dummy solution@." name;
                [||] (* dummy solution *)
            in
            solution_to_file ("solutions/all/"^name^"_"^file) solution;
            let score = score problem solution in
            Format.printf "    Score = %d@." score;
            (score, name, solution))
          strategies
      in
      match List.sort neg_compare scores with
      | (score, name, solution) :: _ ->
         Format.printf "  Taking %s's solution@." name;
         total_score := !total_score + score;
         solution_to_file ("solutions/bests/"^file) solution
      | _ -> assert false)
    files;
  Format.printf "Total score: %d@." !total_score;
  print_endline "==========[ Boloss Out ]=========="
