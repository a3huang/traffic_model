#include <R.h>

// matrices m[i,j] in R are written as m[i + numRow*(j-1)] in C
void
movebluecars(int *a, int *nRow, int *nCol, int *blue, int *nrowBlue){
int numRow = nRow[0],
    numCol = nCol[0],
    numrowBlue = nrowBlue[0],
    i;

int cur_x, cur_y;

for(i = 0; i < numrowBlue; i++){

    // setting current position ; need to subtract 1 from x coordinate since we are counting from 0 here
    cur_x = blue[i] - 1;
    cur_y = blue[i + numrowBlue];

    // for moving blue cars on edge
	if(cur_y == numCol && a[cur_x] == 0){
	   a[cur_x] = 2;
	   a[cur_x + numRow*(cur_y - 1)] = 0;
	   blue[i + numrowBlue] = 1;
    }

	// for moving blue cars on the inside
	else if(cur_y != numCol && a[cur_x + numRow*cur_y] == 0){
        a[cur_x + numRow*cur_y] = 2;
        a[cur_x + numRow*(cur_y-1)] = 0;
        blue[i + numrowBlue] = blue[i + numrowBlue] + 1;
    }
}
}

// matrices m[i,j] in R are written as m[i + numRow*(j-1)] in C
void
moveredcars(int *a, int *nRow, int *red, int *nrowRed){
int numRow = nRow[0],
    numrowRed = nrowRed[0],
    i;

int cur_x, cur_y;

for (i = 0; i < numrowRed; i++){

    // setting current position ; need to subtract 1 from x coordinate since we are counting from 0 here
    cur_x = red[i] - 1;
    cur_y = red[i + numrowRed];

    // for moving red cars on edge
    if(cur_x == numRow - 1 && a[numRow*(cur_y - 1)] == 0){
        a[numRow*(cur_y - 1)] = 1;
        a[numRow - 1 + numRow*(cur_y - 1)] = 0;
        red[i] = 1;
    }

    // for moving red cars on the inside
    else if(cur_x != numRow - 1 && a[cur_x + 1 + numRow*(cur_y - 1)] == 0){
        a[cur_x + 1 + numRow*(cur_y - 1)] = 1;
        a[cur_x + numRow*(cur_y - 1)] = 0;
        red[i] = red[i] + 1;
    }
}
}

// take the above and combine into a single function ; implement the entire runBMLGrid function in C
void
runGrid(int *a, int *nRow, int *nCol, int *blue, int *nrowBlue, int *red, int *nrowRed, int *numSteps){

/*
int numRow = nRow[0],
    numCol = nCol[0],
    numrowRed = nrowRed[0],
    numrowBlue = nrowBlue[0],
    steps = numSteps[0],
    j;
*/

int steps = numSteps[0],
    j;

if(steps % 2 != 0){
   for(j = 0; j < (steps-1)/2; j++){
      movebluecars(a, nRow, nCol, blue, nrowBlue);
      moveredcars(a, nRow, red, nrowRed);
    }
    movebluecars(a, nRow, nCol, blue, nrowBlue);
    return;
}
else if (steps % 2 == 0){
   for(j = 0; j < steps/2; j++){
      movebluecars(a, nRow, nCol, blue, nrowBlue);
      moveredcars(a, nRow, red, nrowRed);
   }
}
}
