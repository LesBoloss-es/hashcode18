
open Common

let neg_compare x y = - compare x y

let strategies : (string * (problem -> solution)) list =
  let strategies = ref [] in
  let m_bonus = 6 in
  for m_tostart = 1 to 3 do
    let m_tostart = 2 * m_tostart in
    for m_wait = 4 to 6 do
      let m_wait = m_wait in
      strategies :=
        (
          (Format.sprintf "Multipliers %d %d %d %d" m_bonus 0 m_tostart m_wait) ,
          Naive.schedule_with_score (Linear.multipliers m_bonus 0 m_tostart m_wait)
        )
        :: !strategies
    done
  done;
  !strategies

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
