// Example inspired from the pigeonhole principle.  The assertion checks that
// all pigeons are accounted for.

int pigeon ()
{
	int pigeon, n1, n2, n3, n4;

	pigeon = 0;
	n1 = 0;
	n2 = 0;
	n3 = 0;
	n4 = 0;

	while (1) {
		pigeon++;
		if (*)
			n1++;
		else
			if (*)
				n2++;
			else
				if (*)
					n3++;
				else
					n4++;
		assert (pigeon == n1 + n2 + n3 + n4);
	}
}
