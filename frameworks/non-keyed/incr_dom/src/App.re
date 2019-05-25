open! Core_kernel;
open Incr_dom;
open Elements;
open Util;

module Model = {
  [@deriving (sexp, fields, compare)]
  type t = {
    data: Int.Map.t(string),
    selected: option(int),
  };

  module Updates = {
    let create_some = (model, n) => {
      let newdata = Util.build_data(n);
      {...model, data: newdata};
    };

    let add_some = (model, n) => {
      let newdata = Util.build_data(n);

      let merge:
        (
          ~key: int,
          [ | `Both(string, string) | `Left(string) | `Right(string)]
        ) =>
        sexp_option('a) =
        (~key as _) =>
          fun
          | `Both(_, _) => failwith("Unexpected duplicate")
          | `Left(a) => Some(a)
          | `Right(a) => Some(a);

      {...model, data: Int.Map.merge(model.data, newdata, ~f=merge)};
    };

    let update_every_10 = model => {
      {...model, data: Int.Map.mapi(model.data, ~f=exclaim)};
    };

    let select = (model, idx) => {
      {...model, selected: Some(idx)};
    };

    let swap_rows = model =>
      if (Int.Map.length(model.data) > 998) {
        let elem_1 = Int.Map.find_exn(model.data, 1);
        let elem_2 = Int.Map.find_exn(model.data, 998);
        let newdata =
          Int.Map.set(model.data, ~key=1, ~data=elem_1)
          |> Int.Map.set(_, ~key=998, ~data=elem_2);
        {...model, data: newdata};
      } else {
        model;
      };

    let remove_item = (model, idx) => {
      let newdata = Int.Map.remove(model.data, idx);
      switch (model.selected) {
      | Some(n) when n == idx => {selected: None, data: newdata}
      | _ => {...model, data: newdata}
      };
    };
  };

  let empty = {data: Int.Map.empty, selected: None};

  let cutoff = (t1, t2) => compare(t1, t2) == 0;
};

module Action = {
  [@deriving sexp]
  type t =
    | RUN
    | RUNLOTS
    | ADD
    | UPDATEEVERYTENTH
    | SELECT(int)
    | REMOVE(int)
    | CLEAR
    | SWAPROWS;

  let should_log = _ => is_debug;
};

module State = {
  type t = unit;
};

let apply_action = (model, action, _, ~schedule_action as _) =>
  switch ((action: Action.t)) {
  | RUN => Model.Updates.create_some(model, 1000)
  | RUNLOTS => Model.Updates.create_some(model, 10000)
  | ADD => Model.Updates.add_some(model, 1000)
  | UPDATEEVERYTENTH => Model.Updates.update_every_10(model)
  | SELECT(item) => Model.Updates.select(model, item)
  | SWAPROWS => Model.Updates.swap_rows(model)
  | REMOVE(item) => Model.Updates.remove_item(model, item)
  | CLEAR => Model.empty
  };

let update_visibility = m => m;

let on_startup = (~schedule_action as _, _) => Async_kernel.return();

let on_display = (~old as _, _, _) => ();

let view = (model: Incr.t(Model.t), ~inject) => {
  open Incr.Let_syntax;

  let sender = (action, _) => inject(action);

  let jumbotron =
    Action.(
      <Jumbotron
        run={sender(RUN)}
        runLots={sender(RUNLOTS)}
        add={sender(ADD)}
        update={sender(UPDATEEVERYTENTH)}
        clear={sender(CLEAR)}
        swapRows={sender(SWAPROWS)}
      />
    );

  let is_selected =
    // Turns out this works!
    switch%map (model >>| Model.selected) {
    | None => (_ => false)
    | Some(n) => (idx => idx == n)
    };

  let%map rows =
    Incr.Map.mapi'(
      model >>| Model.data,
      ~f=(~key as rowid, ~data as rowlabel) => {
        let%map rowlabel = rowlabel
        and is_selected = is_selected;

        Action.(
          <Row
            //  NOTE: Missing the 'key' here, not sure if this is required
            onSelect={sender(SELECT(rowid))}
            onRemove={sender(REMOVE(rowid))}
            selected={is_selected(rowid)}
            rowid
            rowlabel
          />
        );
      },
    );

  <div className="container">
    jumbotron
    <table className="table table-hover table-striped test-data">
      <tbody> ...{Int.Map.data(rows)} </tbody>
    </table>
    <span className="preloadicon glyphicon glyphicon-remove" ariaHidden=true />
  </div>;
};

let create = (model: Incr.t(Model.t), ~old_model as _, ~inject) => {
  open Incr.Let_syntax;
  let%map apply_action = {
    let%map model = model;
    apply_action(model);
  }
  and view = view(model, ~inject)
  and model = model;

  Component.create(~apply_action, model, view);
};
