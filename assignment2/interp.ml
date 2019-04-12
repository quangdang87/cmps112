(* Quang Dang *)
(* qvdang *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with
        | Arrayref (ident, expr2) -> 
            (Hashtbl.find Tables.array_table ident).
                ((int_of_float (eval_expr expr2)))
        | Variable ident -> 
            (Hashtbl.find Tables.variable_table ident))
    | Unary (oper, expr) -> 
        ((Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr)) 
    | Binary (oper, expr1, expr2) ->
        ((Hashtbl.find Tables.binary_fn_table oper) 
            (eval_expr expr1) (eval_expr expr2))   

let eval_bool_expr (expr : Absyn.expr ) : bool = match expr with
    | Number number -> false
    | Unary (oper, expr) -> false
    | Memref memref -> false
    | Binary (oper, expr1, expr2) -> 
        ((Hashtbl.find Tables.binary_bool_table oper) 
            (eval_expr expr1) (eval_expr expr2))

let interp_let ((memref : Absyn.memref), 
    (expr: Absyn.expr)) = match memref with
    | Arrayref (ident, expr2) -> 
        let arr = (Hashtbl.find Tables.array_table ident) in
            if ((Array.length arr) >= 
                (int_of_float (eval_expr expr2))) then
                arr.((int_of_float (eval_expr expr2))) 
                <- (eval_expr expr)
            else raise (Unimplemented "Out of array's range")
    | Variable ident -> 
        (Hashtbl.add Tables.variable_table ident (eval_expr expr))

let interp_dim ((ident : Absyn.ident), (expr : Absyn.expr)) = 
    Hashtbl.replace Tables.array_table ident 
        (Array.make (int_of_float (eval_expr expr) + 1) 0.0)

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

let interp_input (memref_list : Absyn.memref list) =
    let input_number memref = 
        try let number = Etc.read_number () 
            in match memref with
            | Variable var -> 
                Hashtbl.add Tables.variable_table var number
            | Arrayref (ident, expr2) -> 
                let arr = 
                    (Hashtbl.find Tables.array_table ident) in
                    if ((Array.length arr) 
                        > (int_of_float (eval_expr expr2))) then
                        arr.((int_of_float (eval_expr expr2)))
                        <- number
                    else raise 
                       (Unimplemented "Out of array's range")
        with End_of_file -> 
            Hashtbl.replace Tables.variable_table "eof" 1.0
    in List.iter input_number memref_list

let interp_goto (labls : Absyn.label)  = 
    Hashtbl.find Tables.label_table labls

let interp_if ((expr : Absyn.expr), (label : Absyn.label )) = 
    if (eval_bool_expr expr) then Some (interp_goto label)
    else None

let interp_stmt (stmt : Absyn.stmt) :
    Absyn.program option = match stmt with
    | Dim (ident, expr) -> (interp_dim (ident, expr); None) 
    | Let (memref, expr) -> (interp_let (memref, expr); None)
    | Goto labsl -> Some (interp_goto labsl)
    | If (expr, label) -> interp_if (expr, label)
    | Print print_list -> (interp_print print_list; None)
    | Input memref_list -> (interp_input memref_list; None)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> let next_line = interp_stmt stmt in
        match next_line with
            | None -> interpret otherlines
            | Some line -> interpret line 
let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

