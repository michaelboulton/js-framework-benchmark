open! Core_kernel;

let is_debug = false;

let adjectives = [|
  "pretty",
  "large",
  "big",
  "small",
  "tall",
  "short",
  "long",
  "handsome",
  "plain",
  "quaint",
  "clean",
  "elegant",
  "easy",
  "angry",
  "crazy",
  "helpful",
  "mushy",
  "odd",
  "unsightly",
  "adorable",
  "important",
  "inexpensive",
  "cheap",
  "expensive",
  "fancy",
|];

let colours = [|
  "red",
  "yellow",
  "blue",
  "green",
  "pink",
  "brown",
  "purple",
  "brown",
  "white",
  "black",
  "orange",
|];

let names = [|
  "table",
  "chair",
  "house",
  "bbq",
  "desk",
  "car",
  "pony",
  "cookie",
  "sandwich",
  "burger",
  "pizza",
  "mouse",
  "keyboard",
|];

module RowItem = {
  // module Id = {
  //   include Unique_id.Int({});
  // };

  [@deriving (sexp, fields, compare)]
  type t = {
    id: int,
    label: string,
    selected: bool,
  };
};

let build_data_impl = () => {
  let state = ref(1);

  let makeitem = n => {
    (
      n + state^,
      RowItem.{
        id: n + state^,
        selected: false,
        label:
          Array.random_element_exn(adjectives)
          ++ " "
          ++ Array.random_element_exn(colours)
          ++ " "
          ++ Array.random_element_exn(names),
      },
    );
  };

  let impl = count => {
    let generated =
      Int.Map.of_increasing_iterator_unchecked(~len=count, ~f=makeitem);
    state := state^ + count;
    generated;
  };

  impl;
};

let build_data = build_data_impl();

let exclaimi = item => RowItem.{...item, label: item.label ++ " !!!"};

let exclaim = (~key, ~data, existing) =>
  if (0 == (key - 1) mod 10) {
    Int.Map.set(existing, ~key, ~data=exclaimi(data));
  } else {
    existing;
  };
