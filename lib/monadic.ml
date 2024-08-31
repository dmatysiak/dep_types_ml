let ( >>= ) = Result.bind
let ( let* ) x f = Result.bind x f
