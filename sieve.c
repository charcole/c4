sieve ()
{
  char *flags;
  int iter; 
  int count;
  int i;
  int prime;
  int k;

  flags=malloc(8191);
  iter=1;
  while (iter<=10)
  {
	  count = 0;

	  i=0;
	  while (i <= 8190)
	  {
		  flags [i] = 1;
		  i++;
	  }

	  i=0;
	  while (i <= 8190)
	  {
		  if (flags [i]) 
		  {
			  prime = i + i + 3;
			  k = i + prime;

			  while (k <= 8190)
			  {
				  flags [k] = 0;
				  k = k + prime;
			  }

			  count++;
		  }
		  i++;
	  }
	  iter++;
  }

  return count;
}

main ()
{
  int ans;
  ans = sieve ();
  printf("Got: %d\n", ans);
  if (ans != 1899)
    printf("Sieve result wrong, ans = %d, expected 1899\n", ans);
}
