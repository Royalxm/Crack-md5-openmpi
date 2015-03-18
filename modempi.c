#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <mpi.h>

char combi[50];
char md5z[] ="594f803b380a41396ed63dca39503542";
int len;
char mots[100];
char mots2[100];
unsigned long long where=0;
unsigned long long plusplus=1000;
int bool =0;
int bool2=0;
int bool3=0;



 int size, rank; 
unsigned long int nbcombi;

typedef union uwb {
    unsigned w;
    unsigned char b[4];
} MD5union;
 
typedef unsigned DigestArray[4];
 
unsigned func0( unsigned abcd[] ){
    return ( abcd[1] & abcd[2]) | (~abcd[1] & abcd[3]);}
 
unsigned func1( unsigned abcd[] ){
    return ( abcd[3] & abcd[1]) | (~abcd[3] & abcd[2]);}
 
unsigned func2( unsigned abcd[] ){
    return  abcd[1] ^ abcd[2] ^ abcd[3];}
 
unsigned func3( unsigned abcd[] ){
    return abcd[2] ^ (abcd[1] |~ abcd[3]);}
 
typedef unsigned (*DgstFctn)(unsigned a[]);
 

 
unsigned *calctable( unsigned *k)
{
    double s, pwr;
    int i;
 
    pwr = pow( 2, 32);
    for (i=0; i<64; i++) {
        s = fabs(sin(1+i));
        k[i] = (unsigned)( s * pwr );
    }
    return k;
}
 

unsigned rol( unsigned r, short N )
{
    unsigned  mask1 = (1<<N) -1;
    return ((r>>(32-N)) & mask1) | ((r<<N) & ~mask1);
}
 
unsigned *md5( const char *msg, int mlen)
{
    /*Initialize Digest Array as A , B, C, D */
    static DigestArray h0 = { 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476 };
    static DgstFctn ff[] = { &func0, &func1, &func2, &func3 };
    static short M[] = { 1, 5, 3, 7 };
    static short O[] = { 0, 1, 5, 0 };
    static short rot0[] = { 7,12,17,22};
    static short rot1[] = { 5, 9,14,20};
    static short rot2[] = { 4,11,16,23};
    static short rot3[] = { 6,10,15,21};
    static short *rots[] = {rot0, rot1, rot2, rot3 };
    static unsigned kspace[64];
    static unsigned *k;
 
    static DigestArray h;
    DigestArray abcd;
    DgstFctn fctn;
    short m, o, g;
    unsigned f;
    short *rotn;
    union {
        unsigned w[16];
        char     b[64];
    }mm;
    int os = 0;
    int grp, grps, q, p;
    unsigned char *msg2;
 
    if (k==NULL) k= calctable(kspace);
 
    for (q=0; q<4; q++) h[q] = h0[q];   // initialize
 
    {
        grps  = 1 + (mlen+8)/64;
        msg2 = malloc( 64*grps);
        memcpy( msg2, msg, mlen);
        msg2[mlen] = (unsigned char)0x80; 
        q = mlen + 1;
        while (q < 64*grps){ msg2[q] = 0; q++ ; }
        {
            MD5union u;
            u.w = 8*mlen;
            q -= 8;
            memcpy(msg2+q, &u.w, 4 );
        }
    }
 
    for (grp=0; grp<grps; grp++)
    {
        memcpy( mm.b, msg2+os, 64);
        for(q=0;q<4;q++) abcd[q] = h[q];
        for (p = 0; p<4; p++) {
            fctn = ff[p];
            rotn = rots[p];
            m = M[p]; o= O[p];
            for (q=0; q<16; q++) {
                g = (m*q + o) % 16;
                f = abcd[1] + rol( abcd[0]+ fctn(abcd) + k[q+16*p] + mm.w[g], rotn[q%4]);
 
                abcd[0] = abcd[3];
                abcd[3] = abcd[2];
                abcd[2] = abcd[1];
                abcd[1] = f;
            }
        }
        for (p=0; p<4; p++)
            h[p] += abcd[p];
        os += 64;
    }
    return h;
}   

/*
.
.
.
CODE MD5 VOIR LE NORMAL.C 
.
.
.
*/


int md5a(char *mots)
{
    int j,k;
    char aa[102];
    char bb[102];
	bb[0] = 0;
    unsigned *d = md5(mots, strlen(mots));
    MD5union u;
    for (j=0;j<4; j++){
        u.w = d[j];
        for (k=0;k<4;k++) {
	  sprintf(aa,"%02x",u.b[k]);
	   strcat(bb,aa);
	  
	}
 }

	if(strcmp(md5z,bb) == 0)
	{
	  printf("\npassword : %s by pros : %d\n ",mots,rank);
	MPI_Bcast(&bool2,1,MPI_INT, rank,MPI_COMM_WORLD);
	MPI_Finalize(); 
	exit(0);
		return 1;
	}
	else
	{
		return 0;
	}
}


void brute_combi(char * str, char * tmp, int len, int ind,unsigned long long start) {
  int i;
	int a;
  if (ind >= len) {
     if(strcmp(mots,tmp) == 0 || start == 0 || bool2 == 0)
       		{
	 			bool = 1;
	   	
       		}
   			 if( bool == 1)
      		 {
	 			a =  md5a(tmp);
				bool2 = a;
				where++;
				if(where == plusplus)
					  {
					    plusplus = plusplus + plusplus;
					    printf("%llu %s %d\n",where,tmp,rank);
						printf(".\n");
					  }
					
	 			return;
      		 }
   			 else
      {
	return;
      }
  }
 if(bool2 == 0)
{
  for (i = 0; i < strlen(combi); i++) {
    tmp[ind] = str[i];
	if(rank > 0 && bool3 == 0)
	{
		stpcpy(tmp,mots);
		bool3 =1;
	}
    brute_combi(str, tmp, len, ind+1,start);
  }
}
}


 void bruteforce(unsigned long long depart,unsigned long long arrive)
{

  char * tmp = calloc(len + 1, sizeof(char));
  brute_combi(combi, tmp, len, 0,depart);
	
}





int main(int argc, char **argv)
{
	 int converted_number[100];
	char base_digits[100];
	unsigned long long number_to_convert;
	  int next_digit, base, index=0;
	  base = 26;
	
	
	if(argc == 4)
	{
		unsigned long long start_i = 0, stop_i = 0;
		int *sum_buf = NULL;
				MPI_Init(&argc, &argv);
			  MPI_Comm_size(MPI_COMM_WORLD, &size);
			  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

			strcpy(combi, argv[1]);
				   strcpy(md5z, argv[3]);
					len = atoi(argv[2]);
			nbcombi=strlen(combi); //strlencombi pow len
			nbcombi=pow(nbcombi,len);
			 start_i = nbcombi * rank / size;
				  stop_i = nbcombi * (rank + 1) / size;
				where = start_i + 100;
				plusplus = where +1000;
				
				//conversion base decimal vers alpha
				  number_to_convert = start_i;
					base = strlen(combi);
					strcpy(base_digits,combi);

				   /* convert to the indicated base */
				   while (number_to_convert != 0)
				   {
					 converted_number[index] = number_to_convert % base;
					 number_to_convert = number_to_convert / base;
					 ++index;
				   }
				   int i;
				   /* now print the result in reverse order */
				   --index;  /* back up to last entry in the array */
				   i=0;
				   for(  ; index>=0; index--) /* go backward through array */
				   {
				     mots[i] =  base_digits[converted_number[index]];
				     i++;
				   }
				   mots[i] = 0;
				index=0;
 				number_to_convert = stop_i-1;
   while (number_to_convert != 0)
   {
	 converted_number[index] = number_to_convert % base;
	 number_to_convert = number_to_convert / base;
	 ++index;
   }
   /* now print the result in reverse order */
   --index;  /* back up to last entry in the array */
   i=0;
   for(  ; index>=0; index--) /* go backward through array */
   {
     mots2[i] =  base_digits[converted_number[index]];
     i++;
   }
   mots2[i] = 0;
printf("\n[");
printf("%llu %llu %d \n", start_i,stop_i,rank);
			bruteforce(start_i, stop_i); 
			MPI_Finalize(); 
	}
	else {
		printf("mpirun -np 4 ./a.out caract len passwordmd5 \n example : mpirun -np 4 ./a.out abcdefghijklmnopqrstuvwxyz 6 540df7f83845146f0287ff6d2da77900\n");
	}
	
	


}
