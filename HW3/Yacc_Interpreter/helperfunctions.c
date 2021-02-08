#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "helperfunctions.h"



//First element of array stores size
//Second element of array stores capacity
int* createArr(int value){
    int *arr=malloc(50*sizeof(int));
    arr[0]=1;
    arr[1]=50;
    arr[2]=value;
    return arr;
}

int* addArr(int *arr,int value){
    if(arr[0]==arr[1]-2){
        int *temp=malloc(arr[1]*2*sizeof(int));
        for(int i=0;i<arr[1];i++){
            temp[i]=arr[i];
        }
        temp[1]=temp[1]*2;
        free(arr);
        arr=temp;
    }
    arr[arr[0]+2]=value;
    arr[0]++;
    return arr;
}

int set_identifier(struct variables *v,char* identifier,int identifier_value){
    for (int i=0;i<v->arr_size;i++){
        if(strcmp(v->variableList[i],identifier) == 0){
            v->variable_values[i] = identifier_value;
            return identifier_value;
        }
    }
    strcpy(v->variableList[v->arr_size],identifier);
    v->variable_values[v->arr_size] = identifier_value;
    v->arr_size++;
    return identifier_value;
}

int get_identifier(struct variables *v,char* identifier){
    for (int i=0;i<v->arr_size;i++){
        if(strcmp(v->variableList[i],identifier) == 0){
            return v->variable_values[i];
        }
    }
    return -1;
}

int* append(int *arr1,int* arr2){
    if(arr1 == NULL) return arr2;
    if(arr2 ==NULL) return arr1;


    int size=arr1[1]+arr2[1];

    int* result=malloc((size-2)*sizeof(int));

    result[0]=arr1[0]+arr2[0];
    result[1]=size;

    for(int i=2;i<=arr1[0]+1;i++)
        result[i]=arr1[i];
    
    for(int i=2;i<=arr2[0]+1;i++)
        result[i+arr1[0]]=arr2[i];

    free(arr1);
    free(arr2);
    return result;
}


// int main(){
//     int *a=malloc(5*sizeof(int));
//     int *b=malloc(15*sizeof(int));

//     a[0]=3;
//     a[1]=5;
//     a[2]=10;
//     a[3]=11;
//     a[4]=12;

//     b[0]=9;
//     b[1]=15;
//     b[2]=20;
//     b[3]=21;
//     b[4]=22;
//     b[5]=23;
//     b[6]=24;
//     b[7]=25;
//     b[8]=26;
//     b[9]=27;
//     b[10]=28;

//     int *c;
//     c=append(a,b);

//    for(int i=0;i<c[0]+2;i++){
//        printf(" %d",c[i]);
//    }

//     free(c);


// }

// void array_copy(int *arr1,int *arr2,int size){

//     for(int i=0;i<size;i++){

//     }

// }


// struct variables{ 
//     char variableList[1000][50]; //Stores variable names.
//     int variable_values[1000]; //Stores variable values.
//     int arr_size;
// };



// int main(){
//     struct variables v;
//     v.arr_size=0;


//     strcpy(v.variableList[0],"dilara");
//     v.variable_values[0]=5;
//     v.arr_size++;

//     strcpy(v.variableList[1],"aba");
//     v.variable_values[1]=10;
//     v.arr_size++;

//     strcpy(v.variableList[2],"ysf");
//     v.variable_values[2]=15;
//     v.arr_size++;

//     int result=set_identifier(&v,"ysf",2);

//     printf("%d",result);
//     printf("%s=>%d",v.variableList[result],v.variable_values[result]);



//     printf("\n\n\n\n\n");


//     int *a;

//     a=createArr(1);

//     //printf("%d %d %d",a[0],a[1],a[2]);

//     for(int i=0;i<300;i++){
//         a=addArr(a,i);
//     }

//     for(int i=0;i<a[0]+1;i++){
//         printf(" %d ",a[i]);
//     }

// }