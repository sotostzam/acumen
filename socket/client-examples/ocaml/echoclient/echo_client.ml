
      
let echo_client ip p =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in  
  let _ = Unix.connect s (Unix.ADDR_INET ((Unix.inet_addr_of_string ip),p)) in 
    
  let send line o s =  
    output_string o (line^"\n");
    flush o in
    
  let receive i = 
    let line = input_line i in
    let _ = print_endline line in 
      line in
	  
  let i = Unix.in_channel_of_descr s in
  let o = Unix.out_channel_of_descr s in
    
    try
      while(true) do
	let line = receive i in
	  send line o s 
      done
    with e -> ()
      
in 
  if (Array.length Sys.argv) <> 3 
  then 
    (prerr_endline "Usage: ./echo_client <hostname> <port>";
     prerr_endline "   Ex: ./echo_client 127.0.0.1 5000")
  else 
    try  
      echo_client Sys.argv.(1) (int_of_string Sys.argv.(2))
    with e -> prerr_endline "Client entountered an error"
	

 
