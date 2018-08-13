package info.daviot.fpmax

   sealed trait OutMessage {
      def en: String
    }
    object OutMessage {
      case class YouGuessedRight(name: String) extends OutMessage {
        def en: String = "You guessed right, " + name + "!"
      }
      case class YouGuessedWrong(name: String, num: Int) extends OutMessage {
        def en: String = "You guessed wrong, " + name + "! The number was: " + num
      }
      case class DoYouWantToContinue(name: String) extends OutMessage {
        def en: String = "Do you want to continue, " + name + "?"
      }
      case class PleaseGuess(name: String) extends OutMessage {
        def en: String = "Dear " + name + ", please guess a number from 1 to 5:"
      }
      case class ThatIsNotValid(name: String) extends OutMessage {
        def en: String = "That is not a valid selection, " + name + "!"
      }
      case object WhatIsYourName extends OutMessage {
        def en = "What is your name?"
      }
      case class WelcomeToGame(name: String) extends OutMessage {
        def en: String = "Hello, " + name + ", welcome to the game!"
      }
    }
