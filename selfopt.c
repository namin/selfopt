#include <time.h>
#include <stdio.h>
#include <stdlib.h>

typedef int (*funPtr)(int);

int fib(int n)
{
  if (n < 2) return 1;
  else return fib(n-1) + fib(n-2);
}

int fib_iter(int n)
{
  int a = 1;
  int b = 1;
  while (n >= 2) {
    n--;
    int r = a + b;
    a = b;
    b = r;
  }
  return b;
}

int f_iter(int n)
{
  for (int i=0; i<1000*n; i++) {
    fib_iter(n);
  }
  return fib_iter(n);
}

funPtr fs[] = { &fib, &f_iter };

#define F 2
#define I 40
#define FA {0, 0}
#define IA {FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA}
int sk[] = FA;
int st[] = FA;
int sk_per[I][F] = IA;
int st_per[I][F] = IA;

int measure(funPtr f, int arg)
{
  clock_t start, end;
  start = clock();
  f(arg);
  end = clock();
  return end - start + 1;
}

int sample_input()
{
  return rand() % I;
}

int missing(int* sk)
{
  for (int i=0; i<F; i++) {
    if (sk[i]==0) return i;
  }
  return -1;
}

double over(double* sr, int* sk, int* st)
{
  double total = 0.0;
  for (int i=0; i<F; i++) {
    if (st[i] != 0) {
      double r = sk[i]/(1.0*st[i]);
      sr[i] = r;
      total += r;
    }
  }
  return total;
}

double drand(double high)
{
    return (rand() * high)/RAND_MAX;
}

int sample_fun(int* sk, int* st)
{
  int k = missing(sk);
  if (k != -1) return k;
  double sr[] = FA;
  double total = over(sr, sk, st);
  double r = drand(total);
  double c = 0.0;
  for (int i=0; i<F; i++) {
    if (r < c+sr[i]) return i;
    c += sr[i];
  }
  return -1; // shouldn't happen
}

int print_summary_per()
{
  printf("\nSUMMARY\n");
  for (int arg=0; arg<I; arg++) {
    printf("arg=%2d ", arg);
    double sr[] = FA;
    double total = over(sr, sk_per[arg], st_per[arg]);
    if (total > 0.0) {
      for (int i=0; i<F; i++) {
        sr[i] /= total;
      }
    }
    for (int i=0; i<F; i++) {
      printf("%.3f ", sr[i]);
    }
    printf("\n");
  }
  return 0;
}

int pipeline_adaptive(int m, int n)
{
  for (int i=0; i<n; i++) {
    int arg = sample_input();
    int* sk_per_arg = sk_per[arg];
    int* st_per_arg = st_per[arg];
    int k;
    if (i < m ||
        missing(sk_per_arg)!=-1 ||
        missing(st_per_arg)!=-1) {
      k = sample_fun(sk, st);
    } else {
      k = sample_fun(sk_per_arg, st_per_arg);
    }
    int r = measure(fs[k], arg);
    sk[k] += 1;
    st[k] += r;
    sk_per_arg[k] += 1;
    st_per_arg[k] += r;
    if (i % 100 == 1) {
      printf("%d ", i);
      print_summary_per();
    }
  }
  return 0;
}

int main()
{
  pipeline_adaptive(1000, 10000);
  print_summary_per();
  return 0;
}
