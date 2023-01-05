// Dichotomic search.  This code snippet focuses on the case where the target
// is not present.

int dichotomy ()
{
	int x, y;

	if (x < y) {
		while (x < y) {
			if (*)
				x = (x+y) / 2;
			else
				y = (x+y) / 2;
		}
		assert (x == y);
	}
}
