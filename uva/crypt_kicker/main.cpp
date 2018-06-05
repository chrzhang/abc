#include <iostream>
#include <cassert>
#include <set>
#include <array>
#include <vector>
#include <fstream>
#include <sstream>

using namespace std;

set<string> getCandidates(const string & encWord,
                          const set<string> & dictionary,
                          const array<char, 26> currTranslation) {
    set<string> result;
    for (const auto & knownWord : dictionary) {
        if (knownWord.size() != encWord.size()) {
            continue;
        }
        bool canBeTranslated = true;
        for (size_t ii = 0; ii < knownWord.size(); ++ii) {
            const char knownC = knownWord[ii];
            const char encC = encWord[ii];
            const char translation = currTranslation[encC - 'a'];
            if (translation == 0) {
                continue;
            }
            if (translation != knownC) {
                canBeTranslated = false;
                break;
            }
        }
        if (canBeTranslated) {
            result.insert(knownWord);
        }
    }
    return result;
}

bool noOneElseMapsTo(const array<char, 26> & translation, const char ee, const char dd) {
    for (size_t ii = 0; ii < 26; ++ii) {
        if (ii == ee - 'a') {
            continue;
        }
        if (translation[ii] == dd) {
            return false;
        }
    }
    return true;
}

bool updateBasedOn(array<char, 26> & translation,
                   const string & enc,
                   const string & dec) {
    assert(enc.size() == dec.size());
    for (size_t ii = 0; ii < enc.size(); ++ii) {
        if (noOneElseMapsTo(translation, enc[ii], dec[ii]) &&
            (translation[enc[ii] - 'a'] == 0 ||
             translation[enc[ii] - 'a'] == dec[ii])) {
            translation[enc[ii] - 'a'] = dec[ii];
        } else {
            return false;
        }
    }
    return true;
}

bool solveAux(const set<string> & dictionary,
              const set<string>::const_iterator & encrypted_it,
              const set<string> & encrypted,
              array<char, 26> currTranslation,
              array<char, 26> & finTranslation) {
    if (encrypted_it == encrypted.end()) {
        finTranslation = currTranslation;
        return true;
    }
    const string & currEncryptedWord = *encrypted_it;
    const set<string> candidates = getCandidates(currEncryptedWord,
                                                 dictionary,
                                                 currTranslation);
    if (candidates.empty()) {
        return false;
    }
    for (const auto & candidate : candidates) {
        array<char, 26> newTranslation(currTranslation);
        // Update newTranslation based on the candidate
        if (!updateBasedOn(newTranslation, currEncryptedWord, candidate)) {
            continue;
        }
        if (solveAux(dictionary, next(encrypted_it), encrypted, newTranslation, finTranslation)) {
            return true;
        }
    }
    return false;
}

string decode(const vector<string> & encryptedWords, const array<char, 26> & finTranslation) {
    string result = "";
    for (size_t ii = 0; ii < encryptedWords.size(); ++ii) {
        if (ii != 0) {
            result += " ";
        }
        for (size_t jj = 0; jj < encryptedWords[ii].size(); ++jj) {
            result += finTranslation[encryptedWords[ii][jj] - 'a'];
        }
    }
    return result;
}

string toStar(const vector<string> & encryptedWords) {
    string result = "";
    for (size_t ii = 0; ii < encryptedWords.size(); ++ii) {
        if (ii != 0) {
            result += " ";
        }
        result += string(encryptedWords[ii].size(), '*');
    }
    return result;
}

string solve(const set<string> & dictionary,
             const vector<string> & encryptedWords) {
    set<string> encrypted(encryptedWords.begin(), encryptedWords.end());
    array<char, 26> currTranslation, finTranslation;
    currTranslation.fill(0);
    finTranslation.fill(0);
    const bool result = solveAux(dictionary, encrypted.begin(), encrypted, currTranslation, finTranslation);
    if (result) {
        return decode(encryptedWords, finTranslation);
    } else {
        return toStar(encryptedWords);
    }
}

int main() {
    assert("dick and jane and puff and spot and yertle" ==
           solve({"and", "dick", "jane", "puff", "spot", "yertle"},
                 {"bjvg", "xsb", "hxsn", "xsb", "qymm", "xsb", "rqat", "xsb",
                 "pnetfn"}));
    assert("**** *** **** *** **** *** **** *** ******" ==
           solve({"and", "dick", "jane", "puff", "spot", "yertle"},
                 {"xxxx", "yyy", "zzzz", "www", "yyyy", "aaa", "bbbb", "ccc", "dddddd"}));
    ifstream inFile("input.txt");
    int dictLength;
    if (!(inFile >> dictLength)) {
        cerr << "Could not read dictionary length." << endl;
        return 1;
    }
    set<string> currDict;
    for (int ii = 0; ii < dictLength; ++ii) {
        string dictWord;
        if (!(inFile >> dictWord)) {
            cerr << "Could not read dictionary word." << endl;
            return 1;
        }
        currDict.insert(dictWord);
    }
    string currLine;
    getline(inFile, currLine);
    while (getline(inFile, currLine)) {
        vector<string> encWords;
        stringstream ss(currLine);
        string currEncWord;
        while (ss >> currEncWord) {
            encWords.push_back(currEncWord);
        }
        cout << solve(currDict, encWords) << endl;
    }
}
