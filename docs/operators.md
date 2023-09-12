# অপারেটর
ভ্যারিয়েবলে মান এসাইন, দুটি ভ্যালুকে তুলনা করা কিংবা বিভিন্ন গানিতিক অপারেশন সর্ম্পূন  করার জন্য বিভিন্ন অপারেটর ব্যবহার করা হয়। যেমন:

## তুলনা মূলক অপারেটর
তুলনা মূলক স্ট্যাটমেন্ট এ বাক্যের সৌন্দর্য বৃদ্ধিতে একটি অপশনাল কিওয়্যার্ড <b>"হয়"</b> ব্যবহার করা হয়।

|            অপারেটর            |               বিবরন              |              উদাহরন              |
|:-----------------------------:|:--------------------------------:|:--------------------------------:|
|            এর সমান            | দুটি মান সমান কি না               | যদি( ক এর মান খ এর সমান হয় );    |
|               ==              | দুটি মান সমান কি না               | যদি( ক == খ)                     |
|         এর সমান না হয়         | দুটি মান সমান ভিন্ন কি না          | যদি( ক এর মান খ এর সমান না হয় ); |
|              !=               |  দুটি মান সমান ভিন্ন কি না         |  যদি( ক != খ)                    |
| ( হতে, থেকে, চেয়ে )(বড়, বেশি) |  একটির মান অপরটির থেকে বড় কি না  |  যদি(৩ থেকে ৪ বড় হয়)             |
|               >               | একটির মান অপরটির থেকে বড় কি না   | যদি(৪>৩)                         |
| ( হতে, থেকে, চেয়ে )(ছোট, কম) | একটির মান অপরটির থেকে ছোট কি না | যদি(৪ থেকে ৩ ছোট হয়)            |
|               <               | একটির মান অপরটির থেকে ছোট কি না | যদি(৩<৪)                         |
|               >=              | বড় অথবা সমান                     | যদি( ক >= খ )                    |
|               <=              | ছোট অথবা সমান                   | যদি( খ<=ক )                      |

## লজিকাল অপারেটর
| অপারেটর |     বিবরন     |            উদাহরন           |
|:-------:|:-------------:|:---------------------------:|
|   এবং   | আবশ্যক কন্ডিশন  | যদি( ক == ১০ এবং খ == ২০);  |
|    &&   | আবশ্যক কন্ডিশন  | যদি( ক == ১০ && খ == ২০);   |
|   অথবা  | অপশনাল কন্ডিশন | যদি( ক == ১০ অথবা খ == ২০); |
|   &#124;&#124;    | অপশনাল কন্ডিশন | যদি( ক == ১০ &#124;&#124; খ == ২০); |
|    !    | না বোধক      | যদি( ক != ১০)               |

## বুলিয়ান অপারেটর
| অপারেটর |        উদাহরন       |
|:-------:|:-------------------:|
|   সত্য   | যদি( গল্প সত্য হয়);   |
|   হ্যা   | ধরি পরিচিত = হ্যা;   |
|  মিথ্যা  | যদি( গল্প মিথ্যা হয়); |
|    না   | ধরি পরিচিত = না;    |

## গানিতিক অপারেটর
| অপারেটর |        বিবরন       |        উদাহরন       |
|:-------:|:------------------:|:-------------------:|
|    +    |        যোগ        | ক = ১০+২০; // ৩০    |
|    -    |       বিয়োগ       | খ = ২৫ - ৫; // ২০   |
|    ×    |         গুন         | গ = ৫×৫; // ২৫      |
|    *    |         গুন         | গ = ৫*৫; // ২৫      |
|    ÷    |         ভাগ        | ব = ১০০÷২০; // ৫    |
|    /    |         ভাগ        | ব = ১০০/২০; // ৫    |
|    %    |       ভাগশেষ       | ভ = ১৪%৩; // ২      |
|    ++   |  ইনক্রীমেন্ট বা বৃদ্ধি | প = ১০;<br/>প++  ; // ১১ |
|    - -   |  ডিক্রীমেন্ট বা হ্রাস | ম = ১০;<br/>ম- -  ; // ৯ |