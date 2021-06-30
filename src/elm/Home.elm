module Home exposing (view)

-- VIEW


view : Html msg
view =
    div [ class "main_container" ]
        [ viewButton "今までの結果をみる" ClickedHistoryButton
        , viewButton "新規作成" ClickedNewButton
        ]


viewButton : String -> Msg -> Html Msg
viewButton phrase clickedMsg =
    div [ class "button_container", onClick clickedMsg ]
        [ div [ class "button_primary" ] [ text phrase ]
        ]



-- MSG AND UPDATE
