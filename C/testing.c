#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

char *range_extraction(const *args, size_t n)
{
  if (n == 0) {
    return calloc(1, sizeof(char));
  }

  int strSize = 8;
  char *output = calloc(strSize, sizeof(char));
  int index = 0;
  
  int prevNum = args[0];
  int currStreak = 1;
  for (int i = 1; i < n + 1; i++)
  {
    if (i >= n || args[i] - prevNum != 1) {
      char elemStr[25];
      if (currStreak >= 3) {
        sprintf(elemStr, "%d-%d,", args[i - currStreak], args[i - 1]);
      } else if (currStreak == 2) {
        sprintf(elemStr, "%d,%d,", args[i - 2], args[i - 1]);
      } else {
        sprintf(elemStr, "%d,", args[i - 1]);
      }
      int elemLen = strlen(elemStr);
      if (index + elemLen + 1 >= strSize) {
        strSize *= 2;
        output = realloc(output, strSize * sizeof(char));
      }
      strcpy(&output[index], elemStr);
      index += elemLen;
      currStreak = 1;
    } else {
      currStreak++;
    }
    prevNum = args[i];
  }
  
  output[strlen(output) - 1] = '\0';

  return output;
}

int main(int argc, char *argv[])
{
  char *str = range_extraction((const []){-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20}, 20);
  printf("%s\n", str);
  return EXIT_SUCCESS;
}
