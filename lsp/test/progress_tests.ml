open Lsp

let print_round_trip progress =
  let json = Progress.yojson_of_t progress in
  let progress = Progress.t_of_yojson json in
  Progress.yojson_of_t progress |> Yojson.Safe.pretty_to_string |> print_endline
;;

let%expect_test "work-done progress variants round trip through JSON" =
  print_round_trip
    (Progress.Begin
       (Types.WorkDoneProgressBegin.create
          ~title:"Build"
          ~message:"started"
          ~percentage:0
          ~cancellable:true
          ()));
  print_round_trip
    (Progress.Report
       (Types.WorkDoneProgressReport.create
          ~message:"Building [3/10]"
          ~percentage:30
          ~cancellable:false
          ()));
  print_round_trip
    (Progress.End (Types.WorkDoneProgressEnd.create ~message:"Build finished" ()));
  [%expect
    {|
    {
      "kind": "begin",
      "cancellable": true,
      "message": "started",
      "percentage": 0,
      "title": "Build"
    }
    {
      "kind": "report",
      "cancellable": false,
      "message": "Building [3/10]",
      "percentage": 30
    }
    { "kind": "end", "message": "Build finished" } |}]
;;
