/*
    misc.c
    (c) 2015-2016 Greg Sydney-Smith

    Library of useful functions

    Revisions
    2016-09-10 mod FreeNLines() to skip over NULLs
    2016-07-09 +qs2s(), rm_nl2()
    2016-06-27 +StrCat2()
    2016-06-23 iswhite() includes '\r'
    2016-06-16 +FreeNLines()
    2016-06-13 Add getmem();
    2015-00-00 First version
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "misc.h"

char *trim(char *s)
{
    int i;

    for (i=strlen(s); i>0 && s[i-1]==' '; i--) ;
    s[i]='\0';

    return s;
}




void FreeLines(char *lines[])
{
    int i;

    for (i=0; lines[i] != NULL; i++)
        free(lines[i]);
}






char *StrDirFn(const char *dir, const char *fn) {
    char *buff;
    int addslash=FALSE, l1= strlen(dir), l2= strlen(fn);

    if (l1>0 && dir[l1-1] != '\\') addslash=TRUE;
    buff= malloc(l1+l2+(addslash?1:0)+1);

    strcpy(buff,dir);
    if (addslash) strcat(buff,"\\");
    strcat(buff,fn);
    return buff;
}

char *StrMid(char *fm, int start, int len) {
    int i, flen;
    char *to;

    to= malloc(len+1);
    *to= '\0';
    flen = strlen(fm);
    if (start > flen)
        return to;
    for (i=0; i<len && start+i<flen; i++)
        to[i]= fm[start+i];
    to[i]= '\0';
    return to;
}


int MkDir(char *dir) {
    char *s, *parent;
    
    if (dir == NULL) return FALSE;

    s=dir;
    if (strlen(s)>=2 && s[1]==':') s+=2;		// Windows. Skip "drive :" if present
    if (strcmp(s,"." )==0) return FALSE;
    if (strcmp(s,"..")==0) return FALSE;
    if (strcmp(s,"/" )==0) return FALSE;		// Unix, Linux, MinGW
    if (strcmp(s,"\\")==0) return FALSE;		// Windows
    if (strcmp(s,""  )==0) return FALSE;

    char *pLastSlash = strrchr(dir, '/');
    if (! pLastSlash) pLastSlash = strrchr(dir, '\\');	// try the other one too
    if (pLastSlash) {
        parent=StrMid(dir,0,pLastSlash-dir);
        MkDir(parent);
        free(parent);
    }
	
    // All parent dirs are present. Make the full directory.
    mkdir(dir,0750);						// Linux mkdir(...,0777);
    return TRUE;
}




// 2018-08-22
int FileExists(char *fn) {
    FILE *fp;
    if ((fp=fopen(fn,"r"))==NULL) return FALSE;
    fclose(fp);
    return TRUE;
}

long FileSize(char *fn) {
    FILE *fp;
    long sz;

    if ((fp=fopen(fn,"rb"))==NULL) return -1;
    fseek(fp, 0L, SEEK_END);
    sz = ftell(fp);
    fclose(fp);
    return sz;
}

