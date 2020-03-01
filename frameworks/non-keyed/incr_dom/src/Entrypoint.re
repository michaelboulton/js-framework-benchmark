open! Core_kernel;
open Incr_dom;
open Util;

let () = Start_app.start(
  ~debug=is_debug,
  ~bind_to_element_with_id="main",
  (module App),
  ~initial_model=App.init(),
  // ~initial_model=
  // App.Model.Fields.create(
  //   ~data=Int.Map.empty,
  //   ~selected=ref(App.Model.emptyitem),
  // ),
);
