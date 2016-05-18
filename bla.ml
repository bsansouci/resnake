match (Graphics.wait_next_event [Graphics.Poll]) with
 | Graphics.status. {keypressed} -> print_string "HEYYYYY"
 | _ -> assert false
