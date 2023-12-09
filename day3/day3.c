#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>


char* const input_file = "input.txt";

/* Hardcoded dimensions of my input.txt s*/
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


/** Returns the length of the integer starting at lgn x col.
    Returns 0 if no integer starts there */
int integer_length(int lgn, int col) {
  int l = 0;
  while (col + l < NB_COL && is_digit(schematic[lgn][col+l])) l++;
  return l;
}


/** Checks if a number has an adjacent non-digit non-dot char.
    The number starts at pos lgn x col, and lasts for en characters */
bool is_part_number(int lgn, int col, int len) {
  for (int i = max(0, lgn-1); i <= min(NB_LGN-1, lgn+1); i++) {
    for (int j = max(0, col-1); j <= min(NB_LGN-1, col+len); j++) {
      if (is_symbol(schematic[i][j])) return true;
    }
  }
  return false;
}


/** I think there's better out there. This works. Q&D. */
int string_to_int(int lgn, int col, int len) {
  int integer = 0;
  for (int k = 0; k < len; k++) {
    integer *=10;
    integer += schematic[lgn][col+k] - '0';
  }
  return integer;
}


int part1(void) {
  int sum = 0;
  for (int lgn = 0; lgn < NB_LGN; lgn++) {
    int col = 0;
    while (col < NB_COL) {
      int len = integer_length(lgn, col);
      if (is_part_number(lgn, col, len)) sum += string_to_int(lgn, col, len);
      col += len+1;
    }
  }
  return sum;
}

/** Returns the left-wise len of the int at lgn x col */
int leftwise_integer_length(int lgn, int col) {
  int l = 0;
  while (col + l >= 0 && is_digit(schematic[lgn][col+l])) l--;
  return l;
}


/** Computes the gear ratio of lgn x col */
int gear_ratio(int lgn, int col) {
  bool has_int[3][3] = {};

  /* fst find adjacent cases with ints */
  for (int i = -1; i < 2; i++)
    for (int j = -1; j < 2; j++)
      if (lgn+i >= 0 && lgn+i < NB_LGN && col+j >= 0 && col+j < NB_COL
          && is_digit(schematic[lgn+i][col+j])
         )
        has_int[i+1][j+1] = true;
  has_int[1][1] = false;

  /* then, remove redundancy. */
  for (int i = -1; i < 2; i += 2) { //  Only need upper and bottomer row.
    if (lgn+i < 0 || lgn + i >= NB_LGN) continue;
    for (int j = -1; j < 1; j += 1) { // only two fst columns
      if (col+j < 0 || col+j >= NB_LGN) continue;
      int len = integer_length(lgn+i, col+j);
      if (j == -1) {                                  // INT - ? - ?
        if (len >= 1) has_int[i+1][1] = false;        // INT - same INT - ?
        if (len >= 2) has_int[i+1][2] = false;        // INT - same INT - same INT
      }
      if (j == 0 && len >=1) has_int[i+1][2] = false;     // ? - INT - same INT
    }
  } 
  
  /* Ok that's starting to be waaaaaaaaay to long. 
     C isn't the right tool. I need dictionnaries !! */
  int nb_gear = 0;
  int prod = 1;
  for (int i = -1; i < 2; i++)
    for (int j = -1; j < 2; j++)
      if (has_int[i+1][j+1]
          && lgn+i >= 0 && lgn+i < NB_LGN && col+j >= 0 && col+j < NB_COL
          ) {
        nb_gear += 1;
        int start = col + j + leftwise_integer_length(lgn+i, col+j) + 1;
        int len = integer_length(lgn+i, start);
        prod *= string_to_int(lgn+i, start, len);
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

  printf("%d\n", part1());
  printf("%d\n", part2());

  return EXIT_SUCCESS;
}