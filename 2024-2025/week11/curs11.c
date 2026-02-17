#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct Node
{
  int information;
  struct Node *left;
  struct Node *right;
} Node;

Node *newNode(int info, Node *left, Node *right)
{
  Node *result = (Node *)malloc(sizeof(Node));
  result->information = info;
  result->left = left;
  result->right = right;
  return result;
}

void print(Node *root)
{
  if (root == NULL) {
    printf("Leaf");
  } else {
    printf("%d (", root->information);
    print(root->left);
    printf(") (");
    print(root->right);
    printf(")");
  }
}

typedef enum Direction { L, R } Direction;

Node *goPosition(Node *root, Direction *dirs, int length)
{
  if (root == NULL) {
    fprintf(stderr, "Cannot update NULL.");
    exit(-1);
  }
  if (length == 0) {
    return root;
  } else {
    if (dirs[0] == L) {
      return goPosition(root->left, dirs + 1, length - 1);
    } else {
      assert(dirs[0] == R);
      return goPosition(root->right, dirs + 1, length - 1);
    }
  }
}

void updateRoot(Node *root, int info)
{
  assert(root != NULL);
  root->information = info;
}

void update(Direction *dirs, int length, int info, Node *root)
{
  if (root == NULL) {
    fprintf(stderr, "Cannot update NULL.");
    exit(-1);
  }
  if (length == 0) {
    root->information = info;
  } else {
    if (dirs[0] == L) {
      update(dirs + 1, length - 1, info, root->left);
    } else {
      assert(dirs[0] == R);
      update(dirs + 1, length - 1, info, root->right);
    }
  }
}

int main()
{
  Node *t1 = newNode(5,
                     newNode(3,
                             newNode(1, NULL, NULL),
                             newNode(7, newNode(10, NULL, NULL),
                                     newNode(20, NULL, NULL))),
                     newNode(42, newNode(8, NULL, NULL), NULL));
  print(t1);
  printf("\n");
  Direction dirs[10] = { L, R };
  Direction dirsPrime[10] = { L };
  //update(dirs, 2, 8, t1);
  Node *toUpdate = goPosition(t1, dirs, 2);
  updateRoot(toUpdate, 8);
  Node *toUpdateP = goPosition(toUpdate, dirsPrime, 1);
  updateRoot(toUpdateP, 42);
  //update(dirs, 2, 8, t1);
  print(t1);
  printf("\n");
  return 0;
}
