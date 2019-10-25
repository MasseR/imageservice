{ port = +8000
, dbPath = "/tmp/imageservice"
, services = {
    imgur = "asd"
  }
  , carbon = None { host : Text, port : Integer }
, workers = [
    { subreddits =
      [ "wallpapers"
      , "pics"
      , "aviation"
      , "oldschoolcool"
      , "thewaywewere"
      , "abandonedporn"
      , "militaryporn"
      , "blackpeopletwitter"
      , "earthporn"
      , "skyporn"
      , "me_irl"
      , "itookapicture"
      , "eyebleach"
      , "roomporn"
      , "photoshopbattles"


      , "gentlemanboners"
      , "prettygirls"
      , "girlsmirin"
      , "goddesses"
      , "fitandnatural"
      ]
    }
  ]
}
