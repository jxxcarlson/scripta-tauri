module Parser.Meta exposing (Meta, dummy)


type alias Meta =
    { begin : Int, end : Int, index : Int, id : String }


dummy =
    { begin = 0, end = 0, index = 0, id = "dummyId" }
