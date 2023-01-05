// Classical loop example that illustrates range analysis with widening and
// narrowing.  Taken from Comparing the Galois connection and widening/narrowing
// approaches to abstract interpretation by Cousot and Cousot. PLILP 1992.

int classic ()
{
	int x;
	for (x = 1; x <= 100; x++);
	assert (x < 150);
}
