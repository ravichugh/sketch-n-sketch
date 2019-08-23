open Lang

(*******************************************************************************
 * Identifier generation
 *)

let fresh_ident first_char gamma =
  let extract_number (ident : string) : int option =
    let ident_len =
      String.length ident
    in
      if ident_len > 0 && Char.equal (String.get ident 0) first_char then
        ident
          |> StringLabels.sub ~pos:1 ~len:(ident_len - 1)
          |> int_of_string_opt
      else
        None
  in
  let fresh_number : int =
    gamma
      |> List2.filter_map (fst >> extract_number)
      |> List2.maximum
      |> Option2.map ((+) 1)
      |> Option2.with_default 1
  in
    String.make 1 first_char ^ string_of_int fresh_number

let function_char =
  'f'

let variable_char =
  'x'

let match_char =
  'y'

(*******************************************************************************
 * Term permission helpers
 *)

type term_permission =
  | Must
  | May
  | Not

let parts (k : int) : term_permission list Nondet.t =
  Nondet.pure_bind (Nondet.from_list (List2.range ~low:1 ~high:k)) @@ fun i ->
  List2.repeat (i - 1) Not @ [Must] @ List2.repeat (k - i) May

(*******************************************************************************
 * Caching
 *)

(* Types *)

type term_kind =
  | E
  | I

type gen_input =
  { term_kind : term_kind
  ; term_size : int
  ; rel_binding : type_binding option
  ; goal : gen_goal
  }

(* Hashing *)

let hash ({ term_kind; term_size; rel_binding; goal } : gen_input) : string =
  let rec hash_type (tau : typ) : string =
    match tau with
      | TArr (tau1, tau2) ->
          "[" ^ hash_type tau1 ^ ">" ^ hash_type tau2 ^ "]"

      | TTuple taus ->
          taus
            |> List.map hash_type
            |> String.concat ","
            |> (fun s -> "(" ^ s ^ ")")

      | TData d ->
          d
  in
  let hash_bind_spec (bind_spec : bind_spec) : string =
    match bind_spec with
      | NoSpec -> "."
      | Rec name -> "r:" ^ name
      | Arg name -> "a:" ^ name
      | Dec name -> "d:" ^ name
  in
  let tk_string =
    match term_kind with
      | E -> "E"
      | I -> "I"
  in
  let ts_string =
    string_of_int term_size
  in
  let rb_string =
    match rel_binding with
      | None ->
          ""

      | Some (name, (tau, bind_spec)) ->
          name ^ ";" ^ hash_type tau ^ ";" ^ hash_bind_spec bind_spec
  in
  let goal_string =
    (* Sigma never changes, so no need to keep track of it in the cache *)
    let gamma_string =
      goal.gamma
        |> List.map
             ( fun (name, (tau, bind_spec)) ->
                 name ^ "`" ^ hash_type tau ^ "`" ^ hash_bind_spec bind_spec
             )
        |> String.concat "&"
    in
    let gt_string =
      hash_type goal.goal_type
    in
      gamma_string ^ "$" ^ gt_string
  in
    tk_string ^ "!" ^ ts_string ^ "@" ^ rb_string ^ "#" ^ goal_string

(* Caching *)

let gen_cache : (string, exp Nondet.t) Hashtbl.t =
  Hashtbl.create 100

let lookup (gen_input : gen_input) : exp Nondet.t option =
  gen_input
    |> hash
    |> Hashtbl.find_opt gen_cache

let record (gen_input : gen_input) (solution : exp Nondet.t) : exp Nondet.t =
    Hashtbl.add gen_cache (hash gen_input) solution;
    solution

(*******************************************************************************
 * Term generation
 *)

(* --- Important info about the term generation helpers! ---
 *
 * Do NOT call gen_e, rel_gen_e, gen_i, or rel_gen_i from anywhere EXCEPT inside
 * the actual gen function. The gen function handles caching; no other code
 * should worry about directly manipulating the cache.
 *
 * So, if, for example, genE wants to recursively call itself, it should
 * actually do so indirectly via calling gen with the appropriate arguments.
 *
 * Also, these helpers assume term_size > 0.
 *)

let rec gen_e
  (term_size : int) (goal : gen_goal)
  : exp Nondet.t =
    match goal.gamma with
      | binding :: gamma_rest ->
          Nondet.union
            [ gen
                { term_kind = E
                ; term_size
                ; rel_binding = Some binding
                ; goal = { goal with gamma = gamma_rest }
                }
            ; gen
                { term_kind = E
                ; term_size
                ; rel_binding = None
                ; goal = { goal with gamma = gamma_rest }
                }
            ]

      | [] ->
          Nondet.none

(* A helper for the application part of rel_gen_e *)
and rel_gen_e_app
  (term_size : int)
  ((rel_name, (rel_type, _)) as rel_binding : type_binding)
  ({ gamma; goal_type } as goal : gen_goal)
  : exp Nondet.t =
    let possible_arg_type =
      gamma
        |> List2.filter_map
             ( fun (_, (tau, _)) ->
                 Type.domain_of_codomain ~codomain:goal_type tau
             )
        |> Nondet.from_list
    in
    let possible_partition =
      Nondet.from_list @@
        Int2.partition_permutations
          ~n:(term_size - 1) (* -1 for application *)
          ~k:2
    in
    let combined_gamma =
      rel_binding :: gamma
    in
    let app_combine (head : exp) (arg : exp) : exp option =
      if Type.structurally_decreasing combined_gamma ~head ~arg then
        Some (EApp (head, arg))
      else
        None
    in
      Nondet.bind possible_arg_type @@ fun arg_type ->
      Nondet.bind possible_partition @@ fun partition ->
        match partition with
          (* Will always happen*)
          | [k_head; k_arg] ->
              let head_goal =
                { sigma = goal.sigma
                ; gamma = goal.gamma
                ; goal_type = TArr (arg_type, goal_type)
                }
              in
              let arg_goal =
                { sigma = goal.sigma
                ; gamma = goal.gamma
                ; goal_type = arg_type
                }
              in
                Nondet.bind
                  ( gen
                      { term_kind = E
                      ; term_size = k_head
                      ; rel_binding = None
                      ; goal = head_goal
                      }
                  ) @@ fun head_solution ->
                Nondet.bind
                  ( gen
                      { term_kind = E
                      ; term_size = k_head
                      ; rel_binding = Some rel_binding
                      ; goal = head_goal
                      }
                  ) @@ fun rel_head_solution ->
                Nondet.bind
                  ( gen
                      { term_kind = I
                      ; term_size = k_arg
                      ; rel_binding = None
                      ; goal = arg_goal
                      }
                  ) @@ fun arg_solution ->
                Nondet.bind
                  ( gen
                      { term_kind = I
                      ; term_size = k_arg
                      ; rel_binding = Some rel_binding
                      ; goal = arg_goal
                      }
                  ) @@ fun rel_arg_solution ->
                Nondet.from_list @@
                  List2.filter_somes
                    [ app_combine rel_head_solution arg_solution
                    ; app_combine head_solution rel_arg_solution
                    ; app_combine rel_head_solution rel_arg_solution
                    ]

          | _ ->
              print_endline
                ( "WARNING: integer partition is incorrect size (is "
                ^ string_of_int (List.length partition)
                ^ ", should be 2, called with n = "
                ^ string_of_int (term_size - 1)
                ^ ")"
                );
              Nondet.none

and rel_gen_e
  (term_size : int)
  ((rel_name, (rel_type, _)) as rel_binding : type_binding)
  ({ goal_type } as goal : gen_goal)
  : exp Nondet.t =
    match term_size with
      | 1 ->
          if Type.equal goal_type rel_type then
            Nondet.pure @@
              EVar rel_name
          else
            (* "Focusing" *)
            begin match rel_type with
              | TTuple component_types ->
                  let n =
                    List.length component_types
                  in
                    component_types
                      |> List.mapi Pair2.pair
                      |> List.filter (snd >> Type.equal goal_type)
                      |> List.map (fun (i, _) -> EProj (n, i, EVar rel_name))
                      |> Nondet.from_list

              | _ ->
                  Nondet.none
            end

      (* No unary operators *)
      | 2 ->
          Nondet.none

      (* All applications have size > 2 *)
      | _ ->
        rel_gen_e_app term_size rel_binding goal

and genp_i
  (term_size : int)
  (tp : term_permission)
  (rel_binding : type_binding)
  (goal : gen_goal)
  : exp Nondet.t =
    let (rel_binding', gamma') =
      match tp with
        | Must ->
            (Some rel_binding, goal.gamma)

        | May ->
            (None, rel_binding :: goal.gamma)

        | Not ->
            (None, goal.gamma)
    in
      gen
        { term_kind = I
        ; term_size
        ; rel_binding = rel_binding'
        ; goal = { goal with gamma = gamma' }
        }

and gen_i
  (term_size : int)
  (goal : gen_goal)
  : exp Nondet.t =
    match goal.gamma with
      | binding :: gamma_rest ->
          Nondet.union
            [ gen
                { term_kind = I
                ; term_size
                ; rel_binding = Some binding
                ; goal = { goal with gamma = gamma_rest }
                }
            ; gen
                { term_kind = I
                ; term_size
                ; rel_binding = None
                ; goal = { goal with gamma = gamma_rest }
                }
            ]

      | [] ->
          begin match goal.goal_type with
            | TArr (tau1, tau2) ->
                let f_name =
                  fresh_ident function_char []
                in
                let arg_name =
                  fresh_ident variable_char []
                in
                let possible_body =
                  gen
                    { term_kind = I
                    ; term_size = term_size - 1 (* -1 for lambda *)
                    ; rel_binding = None
                    ; goal =
                        { sigma =
                            goal.sigma
                        ; gamma =
                            [ (arg_name, (tau1, Dec f_name))
                            ; (f_name, (goal.goal_type, Rec f_name))
                            ]
                        ; goal_type =
                            tau2
                        }
                    }
                in
                  Nondet.pure_bind possible_body @@ fun body ->
                    EFix (Some f_name, arg_name, body)

            | TTuple taus ->
                let tuple_size =
                  List.length taus
                in
                let possible_partition =
                  Nondet.from_list @@
                    Int2.partition_permutations
                      ~n:(term_size - 1) (* -1 for tuple *)
                      ~k:tuple_size
                in
                Nondet.bind possible_partition @@ fun partition ->
                  Nondet.map (fun es -> ETuple es) @@
                    Nondet.one_of_each @@
                      List.map2
                        begin fun tau n ->
                          gen
                            { term_kind = I
                            ; term_size = n
                            ; rel_binding = None
                            ; goal =
                                { sigma = goal.sigma
                                ; gamma = []
                                ; goal_type = tau
                                }
                            }
                        end
                        taus
                        partition

            | TData datatype_name ->
                Nondet.bind
                  ( List.assoc_opt datatype_name goal.sigma
                      |> Option2.map Nondet.from_list
                      |> Option2.with_default Nondet.none
                  ) @@ fun (ctor_name, arg_type) ->
                let possible_arg =
                  gen
                    { term_kind = I
                    ; term_size = term_size - 1 (* -1 for constructor *)
                    ; rel_binding = None
                    ; goal =
                        { sigma = goal.sigma
                        ; gamma = []
                        ; goal_type = arg_type
                        }
                    }
                in
                  Nondet.pure_bind possible_arg @@ fun arg ->
                    ECtor (ctor_name, arg)
          end

and rel_gen_i
  (term_size : int)
  (rel_binding : type_binding)
  (goal : gen_goal)
  : exp Nondet.t =
    (* All E-forms are I-forms *)
    let e_option =
      gen
        { term_kind = E
        ; term_size
        ; rel_binding = Some rel_binding
        ; goal
        }
    in
    let i_option =
      match goal.goal_type with
        | TArr (tau1, tau2) ->
            let f_name =
              fresh_ident function_char goal.gamma
            in
            let arg_name =
              fresh_ident variable_char goal.gamma
            in
            let possible_body =
              gen
                { term_kind = I
                ; term_size = term_size - 1 (* -1 for lambda *)
                ; rel_binding = Some rel_binding
                ; goal =
                    { sigma =
                        goal.sigma
                    ; gamma =
                        (arg_name, (tau1, Dec f_name))
                          :: (f_name, (goal.goal_type, Rec f_name))
                          :: goal.gamma
                    ; goal_type =
                        tau2
                    }
                }
            in
              Nondet.pure_bind possible_body @@ fun body ->
                EFix (Some f_name, arg_name, body)

        | TTuple taus ->
            let tuple_size =
              List.length taus
            in
            let possible_partition =
              Nondet.from_list @@
                Int2.partition_permutations
                  ~n:(term_size - 1) (* -1 for tuple *)
                  ~k:tuple_size
            in
            let possible_part =
              parts tuple_size
            in
            Nondet.bind possible_partition @@ fun partition ->
            Nondet.bind possible_part @@ fun part ->
              Nondet.map (fun es -> ETuple es) @@
                Nondet.one_of_each @@
                  List2.map3
                    begin fun tau n tp ->
                      genp_i n tp rel_binding
                        { sigma = goal.sigma
                        ; gamma = goal.gamma
                        ; goal_type = tau
                        }
                    end
                    taus
                    partition
                    part

        | TData datatype_name ->
            Nondet.bind
              ( List.assoc_opt datatype_name goal.sigma
                  |> Option2.map Nondet.from_list
                  |> Option2.with_default Nondet.none
              ) @@ fun (ctor_name, arg_type) ->
            let possible_arg =
              gen
                { term_kind = I
                ; term_size = term_size - 1 (* -1 for constructor *)
                ; rel_binding = Some rel_binding
                ; goal =
                    { sigma = goal.sigma
                    ; gamma = goal.gamma
                    ; goal_type = arg_type
                    }
                }
            in
              Nondet.pure_bind possible_arg @@ fun arg ->
                ECtor (ctor_name, arg)
    in
      Nondet.union [e_option; i_option]

and gen (gen_input : gen_input) : exp Nondet.t =
  if gen_input.term_size <= 0 then
    Nondet.none
  else
    match lookup gen_input with
      | Some solution ->
          solution

      | None ->
          record gen_input @@
            begin match (gen_input.term_kind, gen_input.rel_binding) with
              | (E, None) ->
                  gen_e gen_input.term_size gen_input.goal

              | (E, Some rb) ->
                  rel_gen_e gen_input.term_size rb gen_input.goal

              | (I, None) ->
                  gen_i gen_input.term_size gen_input.goal

              | (I, Some rb) ->
                  rel_gen_i gen_input.term_size rb gen_input.goal
            end

(*******************************************************************************
 * Term generation exports
 *)

let clear_cache _ =
  Hashtbl.reset gen_cache

let up_to_e max_size goal =
  List2.range 1 max_size
    |> List.map
         begin fun term_size ->
           gen
             { term_kind = E
             ; rel_binding = None
             ; term_size
             ; goal
             }
         end
    |> Nondet.union
