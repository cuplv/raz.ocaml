type 'a digit =
	| One of 'a
	| Two of 'a * 'a
	| Three of 'a * 'a * 'a
	| Four of 'a * 'a * 'a * 'a
type 'a node =
	| Node2 of 'a * 'a
	| Node3 of 'a * 'a * 'a
type 'a fingertree = 
	| Empty
  | Single of 'a
  | Deep of 'a digit * 'a node fingertree * 'a digit

