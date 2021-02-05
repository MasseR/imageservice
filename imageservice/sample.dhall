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
      , "mildlyinteresting"
      , "interestingasfuck"
      , "damnthatsinteresting"
      , "beamazed"
      , "realllifeshinies"
      , "thatsinsane"
      , "colorizedhistory"
      , "reallifedoodles"
      , "hybridanimals"
      , "roastme"


      , "thinspo"
      , "gentlemanboners"
      , "prettygirls"
      , "girlsmirin"
      , "goddesses"
      , "fitandnatural"
      ]
    }
  ]
}
