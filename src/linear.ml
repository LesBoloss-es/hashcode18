
open Common

let one time pos problem ride =
  (* bonus + duree - temps_total *)
  (
    if distance pos ride.start <= time + ride.earliest_start then
      problem.bonus
    else
      0
  )
  - distance pos ride.start
