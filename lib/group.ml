type t = {
  name : string;
  properties : Property.t list;
  channels : Channel.t list;
}

let find_property_opt name' { properties; _ } =
  List.find_opt (fun { Property.name; _ } -> name = name') properties

let find_property name channel = Option.get (find_property_opt name channel)

let find_channel_opt (channel_name : string) { channels; _ } =
  List.find_opt (fun { Channel.name; _ } -> name = channel_name) channels

let find_channel channel_name group =
  Option.get (find_channel_opt channel_name group)
