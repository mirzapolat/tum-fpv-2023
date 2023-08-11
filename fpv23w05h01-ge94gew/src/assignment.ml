(* Existing definitions from tutorial assignments *)
type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list;
}

type database = student list

let insert s db = s :: db

let rec find_by_id id db =
  match db with
  | [] -> []
  | x :: xs -> if x.id = id then [ x ] else find_by_id id xs

let rec find_by_last_name name db =
  match db with
  | [] -> []
  | x :: xs ->
      if x.last_name = name then x :: find_by_last_name name xs
      else find_by_last_name name xs

(*****************************************************************************)
let remove_by_id id db = List.filter (fun stu -> stu.id <> id) db

let count_in_semester sem db =
  List.fold_left (fun i stu -> if stu.semester = sem then i + 1 else i) 0 db

let student_avg_grade id db =
  match find_by_id id db with
  | [] -> 0.0
  | student :: _ -> (
      let grades = List.map snd student.grades in
      match grades with
      | [] -> 0.0
      | _ ->
          let sum = List.fold_left ( +. ) 0.0 grades in
          let count = float_of_int (List.length grades) in
          sum /. count)

let course_avg_grade course db =
  let filtered_grades =
    List.fold_left (fun acc stu -> let grades = List.filter (fun (c, _) -> c = course) stu.grades in grades @ acc) [] db in
  match filtered_grades with
  | [] -> 0.0
  | grades ->
      let sum = List.fold_left (fun acc (_, grade) -> acc +. grade) 0.0 grades in
      let count = float_of_int (List.length grades) in
      sum /. count
