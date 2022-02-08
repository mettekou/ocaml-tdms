open Angstrom_lwt_unix

let read path = Lwt_io.(with_file ~mode:input path (parse (File.parse path)))
