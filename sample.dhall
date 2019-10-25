{ port = +8000
, dbPath = "/tmp/imageservice"
, services = {
    imgur = "asd"
  }
  , carbon = None { host : Text, port : Integer }
, workers = [{ subreddits = ["wallpapers", "pics"] }] }
