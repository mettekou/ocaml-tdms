open Angstrom

type t = { path : string; properties : Property.t list; groups : Group.t list }

let lead_in_length = 28

let of_objects path objects =
  let groups =
    objects
    |> List.filter_map (function
         | Object.File _ | Object.Channel _ -> None
         | Object.Group (name, properties) ->
             Some
               {
                 Group.name;
                 properties;
                 channels =
                   objects
                   |> List.filter_map (function
                        | Object.File _ | Object.Group _ -> None
                        | Object.Channel (group_name, _, _, _, _) as obj ->
                            if group_name = name then
                              Object.to_channel_opt path obj
                            else None);
               })
  in
  {
    path;
    properties =
      objects
      |> List.find_map (function
           | Object.Channel _ | Object.Group _ -> None
           | Object.File properties -> Some properties)
      |> Option.value ~default:[];
    groups;
  }

let parse path =
  let rec loop objects =
    print_int (List.length objects);
    let* stop = at_end_of_input in
    if stop then return objects
    else
      let* objects' = Segment.parse objects in
      loop objects'
  in
  let+ objects = loop [] in
  of_objects path objects
