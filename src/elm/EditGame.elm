module EditGame exposing (view)

import Html exposing (Html, table, td, text, textarea, th, tr)
import Html.Attributes exposing (class, disabled, maxlength, value)


view : Html msg
view =
    let
        viewEditableTd content =
            td [ class "editGame_td" ] [ textarea [ class "editGame_input", maxlength 6, value content ] [] ]

        viewNotEditableTd content =
            td [ class "editGame_td" ] [ textarea [ class "editGame_input", maxlength 6, value content, disabled True ] [] ]

        viewTh content =
            th [ class "editGame_th" ] [ text content ]

        viewTrTh property player1Name player2Name player3Name player4Name player5Name =
            tr [ class "editGame_tr" ]
                [ viewTh property
                , viewTh player1Name
                , viewTh player2Name
                , viewTh player3Name
                , viewTh player4Name
                , viewTh player5Name
                ]

        viewEditableTrTd roundNumber player1Point player2Point player3Point player4Point player5Point =
            tr [ class "editGame_tr" ]
                [ td [ class "editGame_td" ] [ text roundNumber ]
                , viewEditableTd player1Point
                , viewEditableTd player2Point
                , viewEditableTd player3Point
                , viewEditableTd player4Point
                , viewEditableTd player5Point
                ]

        viewNotEditableTrTd roundNumber player1Point player2Point player3Point player4Point player5Point =
            tr [ class "editGame_tr" ]
                [ td [ class "editGame_td_notEditable" ] [ text roundNumber ]
                , viewNotEditableTd player1Point
                , viewNotEditableTd player2Point
                , viewNotEditableTd player3Point
                , viewNotEditableTd player4Point
                , viewNotEditableTd player5Point
                ]
    in
    table
        [ class "editGame_table" ]
        [ viewTrTh "" "player1" "player2" "player3" "player4" "player5"
        , viewEditableTrTd "1" "12000" "12000" "120000" "12000" "12000"
        , viewEditableTrTd "2" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "3" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "4" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "5" "12000" "12000" "120000" "12000" "12000"
        , viewEditableTrTd "6" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "7" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "8" "12000" "12000" "12000" "12000" "12000"
        , viewNotEditableTrTd "Total" "12000" "12000" "12000" "12000" "12000"
        , viewNotEditableTrTd "チップ" "12000" "12000" "12000" "12000" "12000"
        , viewNotEditableTrTd "収支" "12000" "12000" "12000" "12000" "12000"
        , viewNotEditableTrTd "ゲーム代込" "12000" "12000" "12000" "12000" "12000"
        ]
