# Copyright 2021 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.baseword_name <- "base_word"
.inflection_name <- "inflection"
.prefix_name <- "prefix"
.suffix_name <- "suffix"
.interfix_name <- "interfix"

.lookup_url <- "https://query.data.world/s/x2bjzurhumq3tda6zqaherq6w5vngj"

# The sight word list was derived from the Fry sight words:
# https://sightwords.com/sight-words/fry/#lists
# Words with obvious breakdowns (e.g. "words" -> "word -s") were replaced with
# the base word. A few cases of less-obvious breakdowns were similarly accepted
# (e.g. "length" was excluded from the list to allow the breakdown "long -th").
.fry_sight_words <- sort(readLines("data-raw/sight_words.txt"))

usethis::use_data(
  .baseword_name,
  .inflection_name,
  .prefix_name,
  .suffix_name,
  .interfix_name,
  .lookup_url,
  .fry_sight_words,
  internal = TRUE, overwrite = TRUE
)
rm(.baseword_name)
rm(.inflection_name)
rm(.prefix_name)
rm(.suffix_name)
rm(.interfix_name)
rm(.lookup_url)
rm(.fry_sight_words)
