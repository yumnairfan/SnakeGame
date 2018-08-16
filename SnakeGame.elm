module SnakeGame exposing (..)
import Tuple exposing (..)
import GraphicSVG exposing (..)

bouncyBallRadius: Float
bouncyBallRadius = 5

goalRadius: Float
goalRadius = 7

bouncyBallShape : (Float,Float) -> Shape userMsg
bouncyBallShape pos  = group [
                                circle bouncyBallRadius |> filled blue
                                |> move pos
                            ]

snapshotTimestep = 1

update msg model =
    case msg of
        Tick t _ ->
            let
                (newX,newY) = timeStep (t - model.time) model.position model.velocity
                (newVx,trialVy) = timeStep (t - model.time) model.velocity model.acceleration
            in
                { model | time = t
                        , position = (newX,newY)
                        , velocity = case model.correct of
                                    Just True -> (0,0)
                                    Just False -> (0,0)
                                    Nothing -> (newVx, trialVy)
                        , correct = checkCorrection (checkCollision bouncyBallRadius goalRadius (distance model.position goalPosition)) model.position
                        , lastSnapshot = if (model.time - model.lastSnapshot >= snapshotTimestep) then model.time else model.lastSnapshot
                        , snapshots = model.snapshots ++ if (model.time - model.lastSnapshot >= snapshotTimestep) then [model.position] else []
                }

goalPosition = (60,-40)

timeStep deltaT (x,y) (vx,vy) = (x + deltaT * vx, y + deltaT * vy)

distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance pos posGoal = sqrt <| ((first pos - first posGoal)*(first pos - first posGoal))+((second pos - second posGoal)*(second pos - second posGoal))

checkCollision radBall radGoal distance = if (radBall + radGoal) >= distance then True else False

checkCorrection collision pos  = if collision == True then Just True
                                                else if first pos > 96 || first pos < -96 || second pos > 65 || second pos < -65 then Just False
                                                else Nothing

addPos: (Float, Float) -> (Float, Float) -> (Float, Float)
addPos t1 t2 = ((first t1 + first t2),(second t1 + second t2))

init = { time = 0
        , velocity = velocityInitial
        , position = (-50,0)
        , acceleration = acceleration
        , correct = Nothing
        , lastSnapshot = -snapshotTimestep
        , snapshots = []
        }


main =
    gameApp Tick
        { model = init
        , update = update
        , view = view
        }



view model = collage 192 128 (
                            [ rect 30 45 |> filled grey |> move (-50,-28)   ]
                         ++ (List.map (\(idx1,idx2) -> rect 5 7 |> filled lightYellow
                             |> move (-42 + toFloat idx1, -15 + toFloat idx2))[(0,0),(-8,0),(-16,0),(0,-12),(-8,-12),(-16,-12)])
                         ++ [ rect 10 15 |> filled darkBrown |> move (-50,-43)
                            ,  text "Make the ball reach the goal."
                                |> centered
                                |> filled black
                                |> move (0,50)
                            , case model.correct of
                                Just True -> text "Correct :)" |> centered |> filled green |> move (0,-60)
                                Just False -> text "Please Try again" |> centered |> filled darkRed |> move (0,-60)
                                Nothing -> group []
                            , renderSnapshots model.snapshots
                            , circle goalRadius |> filled green |> move goalPosition
                            , text "GOAL" |> size 4|> filled black |> move (55,-41) |> rotate (degrees 0)
                            , bouncyBallShape model.position
                            , line model.position (addPos model.velocity model.position) |> outlined (dotted 0.5) black
                            ])


renderSnapshots : List (Float,Float) -> Shape userMsg
renderSnapshots snapshots =
    group <| List.map (\(x,y) -> circle 5 |> outlined (dotdash 0.5) black |> move(x,y)) snapshots

type Msg = Tick Float GetKeyState
