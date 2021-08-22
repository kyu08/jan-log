module Expands.Tuple exposing (toIntTuple)

import Expands.String as ExString


toIntTuple : ( String, String ) -> ( Int, Int )
toIntTuple stringTuple =
    Tuple.mapBoth
        ExString.toIntValue
        ExString.toIntValue
        stringTuple
