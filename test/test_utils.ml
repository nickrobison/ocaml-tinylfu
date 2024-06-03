let rec ints step n () = Seq.Cons (n, ints step (n + step))
let at_most = Alcotest.testable Fmt.int (fun l r -> r <= l)

let int_seq start stop ?(step = 1) () =
  let dist = stop - start in
  ints step start |> Seq.take dist
