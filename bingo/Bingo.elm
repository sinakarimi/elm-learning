-- Define our module
module Bingo where

-- Import modules
-- Without exposing any functions we're doing a qualified import
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- Import string module which exposes specific functions
-- By exposing functions we're performing an unqualified import
-- Alternatively we can write exposing (..) to expose all functions
import String exposing (toUpper, repeat, trimRight)

import StartApp.Simple as StartApp

-- newEntry is a "Record" in elm, which is very similar to
-- an object in Javascript. The record is immutable so to change
-- the values of a record we need to create a cloned entry e.g.
-- clonedEntry = { entry | points = 1000, wasSpoken = true }
newEntry phrase points id =
  { phrase = phrase
  , points = points
  , wasSpoken = False
  , id = id
  }

initialModel =
  { entries = 
      [ newEntry "Doing Agile" 200 2
      , newEntry "In the Cloud" 300 3
      , newEntry "Future-Proof" 100 1
      , newEntry "Rockstar Ninja" 400 4
      ]
  }

-- Define a type called Action which has 2 possible values
-- NoOp or Sort. This acts similar to an enumerated type.
type Action 
  = NoOp 
  | Sort 
  | Delete Int

update action model =
  case action of
    NoOp ->
      model

    Sort ->
      -- List.sortBy sorts the model.entries list by
      -- it's "points" property
      -- We can achieve the same result using an anonymous function like so:
      -- List.sortBy (\entry -> entry.points) model.entries
      { model | entries = List.sortBy .points model.entries }

    Delete id ->
      let
          remainingEntries =
            List.filter (\entry -> entry.id /= id) model.entries
      in
        { model | entries = remainingEntries }


-- Define our title function
-- We call the Html.text function and pass in an argument of "Hello, World!"
-- The elm syntax for calling functions is <function name> <arguments> separated by a space
-- We can compose functions using the pipe operator "|>". This will pipe the results of a function into
-- the next function following the pipe operator
title : String -> Int -> Html.Html
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    -- |> String.reverse
    |> trimRight
    |> Html.text

-- Takes 4 string arguments and returns 1 string result
greet : String -> String -> String -> String -> String
greet name color food animal =
  name ++ "'s favorites are: " ++ color ++ " " ++ food ++ " " ++ animal

-- Takes no arguments but returns a page header as an Html type
pageHeader : Html
pageHeader =
  -- Html elements take a list of attributes [ <attribute name> <value> ]
  -- it also takes a list of elements [ element ]
  -- NOTE: An array or "list" is comma separated
  h1 [ id "logo", class "classy" ] [ title "bingo!" 3 ]

pageFooter : Html
pageFooter =
  footer [ ]
    [ a [ href "http://google.com" ]
        [ text "Google" ]
    ]

-- entryItem : newEntry -> Html.Html
entryItem address entry =
  li [ ]
    [ span [ class "phrase" ] [ text entry.phrase ]
    , span [ class "points" ] [ text (toString entry.points) ]
    , button [ class "delete", onClick address (Delete entry.id) ] [ ]
    ]

entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
  in
    ul [ ] entryItems

view address model =
  div [ id "container" ]
    [ pageHeader
    , entryList address model.entries
    , button [ class "sort", onClick address Sort ] [ text "Sort" ]
    , pageFooter
    ]

main =
  -- We can re-write this:
  -- view (update Sort initialModel)
  -- as this (using pipe operator):
  -- initialModel
  --   |> update Sort
  --   |> view
  StartApp.start
    { model = initialModel
    , view = view
    , update = update
    }

