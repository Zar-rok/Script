#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>

#define NBR_FILE_MAX 100
#define LEN_NAME_MAX 256
#define SIGN "#!/bin/bash"

int is_file(const char *file) {
	// Indicate if the file is a standard file or a directory.
	
    struct stat file_stat;
    stat(file, &file_stat);
    return S_ISREG(file_stat.st_mode);
}

char **create_array() {
	// Initialise an array of string.
	
	char **pArray = calloc(NBR_FILE_MAX, sizeof(char *));
	for (size_t file = 0; file < NBR_FILE_MAX; ++file) {
		pArray[file] = calloc(LEN_NAME_MAX, sizeof(char));
	}
	
	return pArray;
}

void destroy_array(char **pArray) {
	// Free all the alocated memory of the array.
	
	for (size_t file = 0; file < NBR_FILE_MAX; ++file) {
		free(pArray[file]);
	}
	free(pArray);
}

char **get_all_bashs(const char *directory) {
	// Get all the name of the script in the given directory.
	
	char **pScripts = create_array();
	
	if (chdir(directory) == -1) {
		perror(directory);
		return pScripts;
	}
	
	DIR *pDir = opendir(directory);	
	if (pDir == NULL) NULL;
	
	struct dirent *pDirent = NULL;
	FILE *pFile = NULL;
	char buf[BUFSIZ];
	
	size_t count = 0;
    while ((pDirent = readdir(pDir)) != NULL && count < NBR_FILE_MAX) {
    
        if (!is_file(pDirent->d_name)) continue;
        
        if ((pFile = fopen(pDirent->d_name, "r+")) == NULL) continue;
        
        fread(buf, strlen(SIGN), sizeof(char), pFile);
        
        if (strncmp(buf, SIGN, strlen(SIGN)) == 0) strncpy(pScripts[count++], pDirent->d_name, LEN_NAME_MAX);

        fclose(pFile);
    }
    
    closedir (pDir);
    
    return pScripts;
}

void print_scripts(char** const pScripts) {
	// Display all the filename of scripts find.
	
	puts("[*] Scripts find :");
	size_t index = 0;
	while (pScripts[index][0] != '\0') {
		printf("\t[-] %s\n", pScripts[index++]);
	}
}

int main(int argc, char** argv) {

	char **pScripts;
	
	if (argc > 1) {
		pScripts = get_all_bashs(argv[1]);
	} else {
		char *pCurrent_dir = getcwd(NULL, 0);
		pScripts = get_all_bashs(pCurrent_dir);
		free(pCurrent_dir);
	}
		
	print_scripts(pScripts);
	
	destroy_array(pScripts);
	
    return 0;
}
