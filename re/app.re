open ReactNative;

type screen =
  | Login
  | MainScreen int;

let compare a b =>
  switch (a, b) {
  | (MainScreen a, MainScreen b) => a == b
  | (Login, Login) => true
  | _ => false
  };

module StackNavigator =
  StackNavigator.Make {
    type navigationState = screen;
    let compare = compare;
  };

type action =
  | Pop
  | Push screen
  | Increment
  | Decrement;

type state = {
  navigationState: list screen,
  count: int
};

let component = ReasonReact.reducerComponent "App";

let renderScreen ::count ::increment ::decrement push::(push: screen => unit) screen =>
  switch screen {
  | MainScreen a =>
    <View>
      (Login.render ::increment ::decrement goToMainScreen::(fun _ => push (MainScreen count)))
      <Text value=(string_of_int a) />
    </View>
  | Login =>
    Login.render ::increment ::decrement goToMainScreen::(fun _ => push (MainScreen count))
  };

let headerTitle screen =>
  switch screen {
  | MainScreen _ => "MainScreen"
  | Login => "Login"
  };

let make _children => {
  ...component,
  initialState: fun () => {navigationState: [Login], count: 0},
  reducer: fun action state =>
    switch action {
    | Increment => ReasonReact.Update {...state, count: state.count + 1}
    | Decrement => ReasonReact.Update {...state, count: state.count - 1}
    | Pop =>
      switch state.navigationState {
      | [_h, ...tail] => ReasonReact.Update {...state, navigationState: tail}
      | _ => ReasonReact.NoUpdate
      }
    | Push screen =>
      ReasonReact.Update {...state, navigationState: [screen, ...state.navigationState]}
    },
  render: fun {state, reduce} =>
    ReasonReact.element @@
    StackNavigator.make
      navigationState::state.navigationState
      goBack::(reduce (fun _ => Pop))
      getHeaderConfig::headerTitle
      render::(
        renderScreen
          count::state.count
          increment::(reduce (fun _ => Increment))
          decrement::(reduce (fun _ => Decrement))
          push::(reduce (fun e => Push e))
      )
};

let root = ReasonReact.wrapReasonForJs ::component (fun jsProps => make jsProps##children);
