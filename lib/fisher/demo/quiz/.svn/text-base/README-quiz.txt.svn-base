This promulgates a cheap Internet quiz.

The external interface is a HTMLlified version of a set of multiple-choice
questions: 

  This tells what kind of a robot you are: 
  
  Your hair color: 
    o black
    o white
    o plaid

  Your eye color: 
    o black
    o purple
    o crimson

  [Tell me what kind of robot I am!]

And, when the button is pressed, it sends back another web page, which might
be: 

   You are a black-haired crimson-eyed robot.


------------------------------------------------------------------------------

Internally, we've got classes:
  - Response: A *mutable* (abstract) class which holds a partial response.
    When the partial response is complete, after processing the last question,
    it is ready to get sent to the user.
  - Question: a pretty boring class holding question text and a list of
    Answers.
  - Answer: One of the possible answers.  This includes both the text/html of
    the answer, and a method 'process(r:Response)' which adds this answer to
    the response that is being built. 
