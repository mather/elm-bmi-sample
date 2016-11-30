import Html exposing (..)
import Html.Attributes as A exposing (type_, value, min, max, step, disabled)
import Html.Events exposing (onInput)
import Json.Decode exposing (decodeString, float)

main = 
  Html.beginnerProgram
    { model  = initialModel
    , view   = view
    , update = update
    }

type alias Model =
  { height : Float
  , weight : Float
  }
  
initialModel : Model
initialModel = 
  { height = 171.5
  , weight = 65.5
  }

type Msg =
  NoOp
  | UpdateHeight Float
  | UpdateWeight Float

update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model
    UpdateHeight newHeight ->
      { model | height = newHeight }
    UpdateWeight newWeight ->
      { model | weight = newWeight }

view : Model -> Html Msg
view model =
  let
    h = model.height / 100 -- メートルに変換
    bmi = model.weight / ( h * h ) 
  in
    div [] [
      form [] [
        heightInput model.height,
        weightInput model.weight
      ],
      p [] [ text ("BMI: " ++ (toString bmi)) ]
    ]
    
heightInput h =
  let
    heightAttributes t = 
      [type_ t
      , value <| toString h
      , A.min "100.0"
      , A.max "250.0"
      , step "0.1"
      , onInput (updateFloat UpdateHeight)
      ]
  in
    p [] [
      label [] [text "身長"],
      input (heightAttributes "range") [],
      input (heightAttributes "number") [], text "cm"
    ]
  
weightInput w =
  let
    weightAttributes t = 
      [ type_ t
      , value <| toString w
      , A.min "30.0"
      , A.max "200.0"
      , step "0.1"
      , onInput (updateFloat UpdateWeight)
      ]
  in
    p [] [
      label [] [text "体重"],
      input (weightAttributes "range") [],
      input (weightAttributes "number") [], text "kg"
    ]
    
updateFloat : (Float -> Msg) -> String -> Msg
updateFloat updater str =
  case decodeString float str of
    Ok f ->
      updater f
    Err _ ->
      NoOp
      