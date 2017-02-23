#include <stdio.h>
#include <string.h>
#include <time.h>
#define MATHLIB_STANDALONE
#include <Rmath.h>

struct card {
  int num;
  char *suit;
};

struct deck {
  struct card cards[52];
  int ind[52];   /*  Indicating how many cards are left in  deck.  */
  int ncards;
};

struct player {
  char name[100];
  int ncards;
  struct card cards[12];
};

void create_deck(struct deck *d);
void print_player(struct player p, int hide);
void deal(struct player *pp, struct deck *dp);
int get_value(struct player p);

int main() {

  struct deck gamedeck;
  struct player computer, human;
  int cont, human_value, computer_value;

  /* setup computer, human */
  strcpy(computer.name, "Dealer");
  computer.ncards = 0;

  printf("Enter your name: ");
  scanf("%s", human.name);
  human.ncards = 0;

  /* dialogue */
  printf("\n\nDealer: Hello, %s! I am your dealer! I am very lucky! So don't cry if you lose! Ha ha!\n\n", human.name);
  printf("...Simple blackjack...\n");
  printf("\t\t 1. Dealer has to take hits until his/her cards total 17 or more points.\n");
  printf("\t\t 2. Player has two options: hit, stand.\n");
  printf("\t\t 3. Ace is counted as 1 or 11 point.\n");
  printf("...Shuffling the cards...\n");

  /* create cards */
  create_deck(&gamedeck);
  set_seed(time(NULL), 580580);

  /* assign two card */
  printf("...dealing first two cards...\n");
  deal(&computer, &gamedeck);
  deal(&computer, &gamedeck);
  deal(&human, &gamedeck);
  deal(&human, &gamedeck);
  print_player(computer, 1);
  print_player(human, 0);

  /* player turn */
  printf("\n\n...PLAYER turn...\n");
  printf("\nEnter your decision (0=stand, 1=hit): ");
  scanf("%d", &cont);
  while (cont){
    deal(&human, &gamedeck);
    print_player(computer, 1);
    print_player(human, 0);
    if (get_value(human)>21){
      printf("Dealer: You lose! Don't cry!\n");
      return 0;
    }
    printf("\nEnter your decision (0=stand, 1=hit): ");
    scanf("%d", &cont);
  }
  
  /* computer turn */
  printf("\n\n...DEALER turn...\n");
  print_player(computer, 0);
  while (get_value(computer)<17){
    printf("\nEnter any number to continue: ");
    scanf("%d", &cont);
    printf("\n");
    deal(&computer, &gamedeck);
    print_player(computer, 0);
    print_player(human, 0);
    if (get_value(computer)>21){
      printf("Dealer: Even though I am lucky, there is still small chance that I lose! You win!\n");
      return 0;
    }
  }

  /* final judge: if both did not bust (>21) */
  human_value = get_value(human);
  computer_value = get_value(computer);
  if (human_value > computer_value){
      printf("Dealer: Even though I am lucky, there is still small chance that I lose! You win!\n");
  } else if (computer_value > human_value) {
      printf("Dealer: You lose! Don't cry!\n");
  } else {
    printf("Dealer: Draw!\n");
  }
  
  return 0;
}

void create_deck(struct deck *dp){
  int i, ind;
  for (i=0; i<52; i++) {
    (dp->cards)[i].num = i % 13 + 1;
    ind = i / 13;
    switch(ind){
      case 0:
        (dp->cards)[i].suit = "heart";
        break;
      case 1:
        (dp->cards)[i].suit = "diamond";
        break;
      case 2:
        (dp->cards)[i].suit = "spades";
        break;
      case 3:
        (dp->cards)[i].suit = "clubs";
    }

    (dp->ind)[i] = 1;
  }
  dp->ncards = 52;
}

void print_player(struct player p, int hide) {
  int i;
  printf("========================\n");
  printf("name: %s\n", p.name);
  printf("total number of cards: %d\n", p.ncards);
  printf("cards:\n");

  for (i=0; i<(p.ncards-hide); i++){

	   switch(p.cards[i].num){ 
		   case 1:
		    printf("\t%s A\n", p.cards[i].suit);
			break;
		   case 11:
		    printf("\t%s J\n", p.cards[i].suit);
			break;
		   case 12:
		    printf("\t%s Q\n", p.cards[i].suit);
			break;
		   case 13:
		    printf("\t%s K\n", p.cards[i].suit);
			break;
		    default:
            printf("\t%s %d\n", p.cards[i].suit, p.cards[i].num);
	   }
  }

  if (hide){
    printf("\t* *\n");
    printf("points: *\n");
  } else {
    printf("points: %d\n", get_value(p));
  }
  printf("========================\n");
}

void deal(struct player *pp, struct deck *dp){
  int i, j, rnum;
  
  /* get a card */
  rnum = (int) ftrunc(unif_rand() * (dp->ncards)); /* uniform over 0,1,..,ncards-1 */
  i = 0;
  j = -1;
  while (j<rnum){
    j += (dp->ind)[i];
    i++;
  }
  i--;
  (pp->cards)[pp->ncards] = (dp->cards)[i];
  pp->ncards += 1;
  (dp->ind)[i] = 0;
  dp->ncards -= 1;
}


int get_value(struct player p){
  int i, value=0;

  for (i=0; i<p.ncards; i++){
    if (p.cards[i].num<=10 && p.cards[i].num>=2){
      value += p.cards[i].num;
    } 
	if (p.cards[i].num<=13 && p.cards[i].num>=11){
      value += 10;
    }
  }

  for (i=0; i<p.ncards; i++){
	  if (p.cards[i].num == 1){

		  if ( (value+11)<=21 ){
			  value +=11;
		  }
		  else {
			  value +=1;
		  }

	  }
  }


  return value;
}
