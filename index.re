open Printf;

open Graphics;

open_graph "";

let busyLoop time cb => {
  let rec busyLoopInner prevTime =>
    if (Sys.time () -. prevTime > time) {
      cb ()
    } else {
      busyLoopInner prevTime
    };
  busyLoopInner (Sys.time ())
};

type positionType = {x: int, y: int};

type directionType = | Up | Down | Left | Right;

type gameStateType = {
  direction: directionType,
  position: list positionType,
  apples: list positionType
};

let getNextPosition currentDirection =>
  if (key_pressed ()) {
    let curChar = read_key ();
    /* let (curX, curY) = current_point ();
       moveto 200 10;
       set_text_size 14;
       draw_string (sprintf "%c" curChar);
       moveto curX curY; */
    switch curChar {
    | 'w' => Up
    | 's' => Down
    | 'a' => Left
    | 'd' => Right
    | _ => currentDirection
    }
  } else {
    currentDirection
  };

let printMessage msg => {
  set_color red;
  let (curX, curY) = current_point ();
  moveto 200 10;
  set_text_size 38;
  draw_string (sprintf "%s" msg);
  moveto curX curY
};

let gridSize = 30;

let radius = gridSize / 2;

let maxX = size_x () / gridSize;

let maxY = size_y () / gridSize;

let step = 1;

let drawSnake allPos => {
  set_color red;
  ignore (
    List.map
      (fun pos => fill_circle (pos.x * gridSize + radius) (pos.y * gridSize + radius) radius)
      allPos
  )
};

let drawApple allPos => {
  set_color green;
  ignore (
    List.map
      (fun pos => fill_circle (pos.x * gridSize + radius) (pos.y * gridSize + radius) radius)
      allPos
  )
};

let detectCollision listOfPositions nextHead => {
  let list = List.filter (fun pos => nextHead.x == pos.x && nextHead.y == pos.y) listOfPositions;
  switch list {
  | [] => None
  | [x] => Some x
  | _ => assert false
  }
};

let rec addRandApple position apples => {
  let x = Random.int maxX;
  let y = Random.int maxY;
  switch (List.filter (fun pos => pos.x == x && pos.y == y) position) {
  | [] => [{x, y}, ...apples]
  | [x] => addRandApple position apples
  | _ => assert false
  }
};

let genNewState () => {
  let newDirection =
    switch (Random.int 4) {
    | 0 => Up
    | 1 => Down
    | 2 => Left
    | 3 => Right
    | _ => assert false
    };
  let newPosition = addRandApple [] [];
  {direction: newDirection, position: newPosition, apples: addRandApple newPosition []}
};

let rec mainLoop i (gameState: gameStateType) => {
  clear_graph ();
  drawSnake gameState.position;
  drawApple gameState.apples;
  let nextDirection = getNextPosition gameState.direction;
  let snakeHead = List.hd gameState.position;
  let nextHead =
    switch nextDirection {
    | Up => {x: snakeHead.x, y: (snakeHead.y + step) mod maxY}
    | Down => {x: snakeHead.x, y: ((snakeHead.y - step) + maxY) mod maxY}
    | Right => {x: (snakeHead.x + step) mod maxX, y: snakeHead.y}
    | Left => {x: ((snakeHead.x - step) + maxX) mod maxX, y: snakeHead.y}
    };
  switch (detectCollision gameState.position nextHead) {
  | None =>
    let apples = gameState.apples;
    let (tail, nextApples) =
      switch (detectCollision apples nextHead) {
      | None => (List.rev (List.tl (List.rev gameState.position)), apples)
      | Some posOfCollision => (
          gameState.position,
          addRandApple
            gameState.position
            (
              List.filter (fun pos => pos.x != posOfCollision.x || pos.y != posOfCollision.y) apples
            )
        )
      };
    let nextPosition = [nextHead, ...tail];
    busyLoop
      (1.0 /. 4.0 /. (float_of_int (List.length nextPosition + 1) /. 2.0))
      (
        fun () =>
          mainLoop (i + 1) {direction: nextDirection, position: nextPosition, apples: nextApples}
      )
  | Some _ => {
    let rec loopAndCountDown x => {
      if (x == 0) {
        mainLoop 0 (genNewState ())
      } else {
        clear_graph ();
        drawSnake gameState.position;
        drawApple gameState.apples;
        printMessage (sprintf "You lose - Restarting in %d seconds" x);
        busyLoop 1.0 (fun () => loopAndCountDown (x - 1));
      }
    };
    loopAndCountDown 5;
  }
  }
};

mainLoop 0 (genNewState ());
