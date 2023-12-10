#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>


char* const input_file = "input.txt";

/* Hardcoded dimensions of my input.txt */
#define NB_LGN 140
#define NB_COL 140

char schematic[NB_LGN][NB_COL];

void fill_schematic(void) {
  FILE* fptr = fopen(input_file, "r");
  if (fptr == NULL) {
    fprintf(stderr, "Can't open file %s. Aborting.\n", input_file);
    exit(EXIT_FAILURE);
  }

  for (int lgn = 0; lgn < NB_LGN; lgn++) {
    for (int col = 0; col < NB_COL; col++) {
      schematic[lgn][col] = fgetc(fptr);
    }
    fgetc(fptr); // for the final \n
  }

  if (fclose(fptr) != 0)
    fprintf(stderr, "Can't close file %s. Continuing anyway.\n", input_file);
  return;
}


/* 4 self-explanatory utilitaries */

int min(int a, int b) {
  return a < b ? a : b;
}
int max(int a, int b) {
  return a > b ? a : b;
}
bool is_digit(char c) {
  return '0' <= c && c <= '9';
}
bool is_symbol(char c) {
  return c != '.' &&  !is_digit(c);
}


/* Compute the leftwise and rightwise lenghts of schematic integers */
// EXAMPLE : "abcd", starting from 'c' : leftwise len = 3, rightise len = 2

typedef struct paire_s {int left; int right;} paire;

paire left_right_len[NB_LGN][NB_COL]; 
  /* the integer at schematic[i][j] 
     extends for len.left char to the left (includind j),
     and to len.right char to the right (excluding j) */
  
/** Computes the leftand right-wise lenghts of lgn x col*/
void compute_left_right(int lgn, int col) {
  int l = 0;
  while (col - l >= 0 && is_digit(schematic[lgn][col-l])) l++;
  
  int r = 0;
  while (col + r < NB_COL && is_digit(schematic[lgn][col+r])) r++;

  left_right_len[lgn][col].left = l;
  left_right_len[lgn][col].right = r;
}

/** Comptes the left and right-wise lengths of all coordinates*/
void fill_left_right_len(void) {
  for (int lgn = 0; lgn < NB_LGN; lgn++)
    for (int col = 0; col < NB_COL; col++)
      compute_left_right(lgn, col);
}


/** Returns the integer at lgn x col. If no integer there, returns 0. */
int get_integer(int lgn, int col) {
  int start = col - left_right_len[lgn][col].left +1;
  int len = left_right_len[lgn][start].right;

  int integer = 0;
  for (int j = 0; j < len; j++) {
    integer *=10;
    integer += schematic[lgn][start+j] - '0';
  }
  return integer;
}


/** Checks if a number has an adjacent non-digit non-dot char.
    The number starts at pos lgn x col, has rightwise length len */
bool is_part_number(int lgn, int col) {
  if (!is_digit(schematic[lgn][col])) return false;

  assert(left_right_len[lgn][col].left == 1); // to help me debug
  int len = left_right_len[lgn][col].right;
  for (int i = max(0, lgn-1); i <= min(NB_LGN-1, lgn+1); i++) {
    for (int j = max(0, col-1); j <= min(NB_LGN-1, col+len); j++) {
      if (is_symbol(schematic[i][j])) return true;
    }
  }
  return false;
}


int part1(void) {
  int sum = 0;
  for (int lgn = 0; lgn < NB_LGN; lgn++) {
    int col = 0;
    while (col < NB_COL) {
      if (is_part_number(lgn, col)) sum += get_integer(lgn, col);
      col += left_right_len[lgn][col].right + 1;
    }
  }
  return sum;
}


/* part 2 */

/** Computes the gear ratio of lgn x col */
int gear_ratio(int lgn, int col) {
  bool has_int[3][3] = {}; /* top left -- top center -- top right
                              left     --     X      -- right
                           bottom left -- bot center -- bottom right */
// schematic[i][j] is at has_int[i-row_X+1][j-col_X+1]

  for (int i = max(0, lgn-1); i <= min(NB_LGN-1, lgn+1); i++) {
    int j = max(0, col-1);
    while (j <= min(NB_COL-1, col +1)) {
      if (is_digit(schematic[i][j])) {
        has_int[i-lgn+1][j-col+1] = true;
        j += left_right_len[i][j].right;
      }
      j++;
    }
  }

  int nb_gear = 0;
  int prod = 1;
  for (int i = max(0, lgn-1); i <= min(NB_LGN-1, lgn+1); i++)
    for (int j = max(0, col-1); j <= min(NB_COL-1, col+1); j++)
      if (has_int[i-lgn+1][j-col+1]) {
        nb_gear += 1;
        int start = j - left_right_len[i][j].left +1;
        prod *= get_integer(i, start);
      }
  
  if (nb_gear != 2) return 0;
  else              return prod;
}


int part2() {
  int sum = 0;
  for (int lgn = 0; lgn < NB_LGN; lgn++)
    for (int col = 0; col < NB_COL; col++)
      if (schematic[lgn][col] == '*')
        sum += gear_ratio(lgn, col);
  return sum;
}


int main(void) {
  fill_schematic();
  fill_left_right_len();

  printf("%d\n", part1());
  printf("%d\n", part2());

  return EXIT_SUCCESS;
}