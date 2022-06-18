/* { dg-do compile} */

#define NULL ((void*)0)
typedef unsigned long size_t;
typedef long intptr_t;
typedef unsigned long uintptr_t;
typedef long scalar_t__;
typedef int bool;
#define false 0
#define true 1

typedef struct TYPE_4__ TYPE_2__;
typedef struct TYPE_3__ TYPE_1__;

struct TYPE_4__
{
  size_t modCount;
  TYPE_1__ *modList;
};

struct TYPE_3__
{
  void *modDescr;
  void *modName;
};

size_t MAX_MODS;
void *String_Alloc (char *);
int test_strlen (char *);
int trap_FD_GetFileList (char *, char *, char *, int);
TYPE_2__ uiInfo;

__attribute__((used)) static void
UI_LoadMods ()
{
  int numdirs;
  char dirlist[2048];
  char *dirptr;
  char *descptr;
  int i;
  int dirlen;

  uiInfo.modCount = 0;
  numdirs = trap_FD_GetFileList ("$modelist", "", dirlist, sizeof (dirlist));
  dirptr = dirlist;
  for (i = 0; i < numdirs; i++)
    {
      dirlen = test_strlen (dirptr) + 1;
      descptr = dirptr + dirlen;
      uiInfo.modList[uiInfo.modCount].modName = String_Alloc (dirptr);
      uiInfo.modList[uiInfo.modCount].modDescr = String_Alloc (descptr);
      dirptr += dirlen + test_strlen (descptr) + 1;
      uiInfo.modCount++;
      if (uiInfo.modCount >= MAX_MODS)
        {
	  break;
        }
    }
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 1 "struct_layout" } } */
