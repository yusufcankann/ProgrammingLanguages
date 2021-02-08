#ifndef __HELPERFUNCTIONS_H__
#define __HELPERFUNCTIONS_H__



struct variables{ 
    char variableList[1000][50]; //Stores variable names.
    int variable_values[1000]; //Stores variable values.
    int arr_size;
};

int* createArr(int value);
int* addArr(int *arr,int value);
int set_identifier(struct variables *v,char* identifier,int identifier_value);
int get_identifier(struct variables *v,char* identifier);
int* append(int *arr1,int* arr2);



#endif