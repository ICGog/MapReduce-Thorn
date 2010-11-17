This is the Cheeper demo, a little Twitter-like thing with a text interface
allowing multiple clients to "chirp" bits of wisdom, and the ability to vote
for or against wisdom.

Run it with the shell scripts: 

(1) In one shell, ./run-server to start the server.

(2) In another shell, ./run-client to start the client.

(3) If you want to see multi-user in action, start a third shell and ./run-2 

Here's a typical Cheeper session: 

~/thorn/fisherws/fisher/demo/cheeper: ./run-client
Welcome to Cheeper!
? for help
Who are you? Bard
Chirp: I like numbers!
You chirped '(0) "I like numbers!" -- Bard'
Chirp: I like exotic Unicode symbols!
You chirped '(1) "I like exotic Unicode symbols!" -- Bard'
Chirp: The '/' command lists all the chirps in the system.
You chirped '(2) "The '/' command lists all the chirps in the system." -- Bard'
Chirp: /
(0) "I like numbers!" -- Bard [+0/-0]
(1) "I like exotic Unicode symbols!" -- Bard [+0/-0]
(2) "The '/' command lists all the chirps in the system." -- Bard [+0/-0]
Chirp: Oh, and '?' gets help
You chirped '(3) "Oh, and '?' gets help" -- Bard'
Chirp: ?
? = help
/ = read
+N = vote for
-N = vote against
other = chirp that
Chirp: You can vote for or against chirps, by number...
You chirped '(4) "You can vote for or against chirps, by number..." -- Bard'
Chirp: +4
Thanks
Chirp: /
(4) "You can vote for or against chirps, by number..." -- Bard [+1/-0]
(0) "I like numbers!" -- Bard [+0/-0]
(1) "I like exotic Unicode symbols!" -- Bard [+0/-0]
(2) "The '/' command lists all the chirps in the system." -- Bard [+0/-0]
(3) "Oh, and '?' gets help" -- Bard [+0/-0]
Chirp: Chirps are printed in order of love: (+ votes) - (- votes)
You chirped '(5) "Chirps are printed in order of love: (+ votes) - (- votes)" -- Bard'
