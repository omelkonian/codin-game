#include <iostream>
#include <string>
#include <deque>
#include <algorithm>

using namespace std;

int compare(char s1, char s2) {
    string order = "234567891JQKA";
    int index1 = order.find_first_of(s1);
    int index2 = order.find_first_of(s2);
    if (index1 == index2) return 0;
    else if (index1 > index2) return -1;
    else return 1;
}

string pop(deque<string> *cards, deque<string> *temp) {
    string front = cards->front();
    temp->push_back(front);
    cards->pop_front();

    return front;
}

string popFront(deque<string> *cards, deque<string> *temp) {
    if (cards->empty()) {
        cout << "PAT" << endl;
        exit(0);
    }
    string front = cards->front();
    temp->push_back(front);
    cards->pop_front();

    return front;
}

int main()
{
    // Data structures
    deque<string> cards1, cards2, temp1, temp2;

    int n; // the number of cards for player 1
    cin >> n; cin.ignore();
    for (int i = 0; i < n; i++) {
        string cardp1; // the n cards of player 1
        cin >> cardp1; cin.ignore();
        cards1.push_back(cardp1);
    }
    int m; // the number of cards for player 2
    cin >> m; cin.ignore();
    for (int i = 0; i < m; i++) {
        string cardp2; // the m cards of player 2
        cin >> cardp2; cin.ignore();
        cards2.push_back(cardp2);
    }

    int rounds = 0;
    while (!(cards1.empty() || cards2.empty())) {
        //  Draw cards
        string card1 = pop(&cards1, &temp1);
        string card2 = pop(&cards2, &temp2);

        // War!!
        while (compare(card1[0], card2[0]) == 0) {
            // Remove 3 cards each
            for (int i = 0; i < 3; i++) {
                popFront(&cards1, &temp1);
                popFront(&cards2, &temp2);
            }
            //  Draw cards
            card1 = popFront(&cards1, &temp1);
            card2 = popFront(&cards2, &temp2);
        }

        if (compare(card1[0], card2[0]) < 0) {
            // Player 1 wins round
            cards1.insert(cards1.end(), temp1.begin(), temp1.end());
            cards1.insert(cards1.end(), temp2.begin(), temp2.end());
        }
        else {
            // Player 2 wins round
            cards2.insert(cards2.end(), temp1.begin(), temp1.end());
            cards2.insert(cards2.end(), temp2.begin(), temp2.end());
        }
        temp1.clear();
        temp2.clear();

        rounds++;
    }
    cout << ((cards2.empty()) ? "1 ":"2 ") << rounds << endl;
}
