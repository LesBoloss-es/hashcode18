
open Common

let one time pos problem ride =
  if time + distance pos ride.start + ride.duration <= ride.latest_finish then
    (* bonus + duree - temps_total *)
    (
      if distance pos ride.start <= time + ride.earliest_start then
        problem.bonus
      else
        0
    )
    - distance pos ride.start
  else
    min_int
