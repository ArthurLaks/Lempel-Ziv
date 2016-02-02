import qualified Data.ByteString.Lazy as BS
import  Data.Binary.Put
import Data.Binary.Get
import qualified Data.Map as M
import System.Environment
import Data.Int
import Data.Tuple
import GHC.Word


--An index in the table
type Index = Word16
--The compressed file is made up of a sequence of index-char pairs that can be
--expanded into strings with a decodingTable.  The second entry is Nothing
--when

type Abbreviation = (Index,Maybe Word8)
--The type of table used when compressing files.
type EncodingTable = M.Map Abbreviation Index

--Compresses enough characters to form an abbreviation.
--Takes a string and a dictionary and returns an abbreviation and the
--remainder of the string.
compressChar :: [Word8] -> EncodingTable -> ([Word8],Abbreviation)
compressChar string  table = compressChar' string 0
--Takes a string of remaining characters and an index representing the
--characters compressed so far and returns the remaining characters and
--the abbreviation that the beginning of the string was compressed to.
        where  compressChar':: [Word8]->Index-> ([Word8],Abbreviation)
               --If the end of the string is encountered after a series of
               --characters in the dictionary, insert the address followed
               --by Nothing.
               compressChar' [] address = ([],(address,Nothing))
               compressChar' (first:rest) address  =
                  case (address,Just first) `M.lookup` table of
                   --If the string so far is in the table, pass its address
                   --and the next character to the recursive call.
                      (Just newAddress) -> compressChar' rest newAddress
                      --If the character is the first one after a sequence
                      --from the table, write the address followed by the
                      --character.
                      Nothing -> (rest,(address,Just first))

-- The encoding table is inistialized with every value between 0 and 255.
initEncodingTable::EncodingTable
initEncodingTable = M.fromList [((0,Just n),fromIntegral (n + 1)) | n <- [0..255]]

--Takes a list of Word8s and compresses them into a list of abbrevations
compressString::[Word8]->[Abbreviation]
compressString = compressString' 256 initEncodingTable where
  --The first argument is the index in the file (and table) that the function is up to.
  --The third argument is the remainder of the string to compress.
  --This function compresses an entry and calls itself on the rest of the
  --list, then appends the entry to the beginning of the resuling list
  --of abbreviations.
  compressString'::Index -> EncodingTable -> [Word8] -> [Abbreviation]
  compressString' _ _ []  = []
  --If the index overflowed and wrapped back to zero then pass the initialized
  --table to the recursive call but leave other arguments unchanged.
  compressString' 0 _ remainder = compressString' 256 initEncodingTable  remainder
  --Compress the beginning of the string to get the first entry and call the
  --function on the rest of the string, then append the first entry to the
  --beginning.
  compressString' index table string = current_abbreviation:(compressString' (index + 1) (M.insert current_abbreviation  index table) remainder) where
    (remainder,current_abbreviation) = compressChar string table
--compressString  index string table = (abbrev:list,if end then table else M.insert abbrev index newTable,if end then addr else 0)
  --  where
    --      (list,newTable,address) = compressString 0 (index + 1) remainder (M.insert abbrev index table)
      --    (remainder,abbrev@(addr,char)) = compressChar string table
        --  end = char == 0

  --Maps an index to an abbreviation.  Allows an index to be expanded into
  --a string.
type DecodingTable = M.Map Index Abbreviation

--Takes an abbreviation and a table and expands it into a string based on the
--table.
decompressEntry::Abbreviation->DecodingTable->[Word8]
--decompressEntry' constructs the string in reverse, so it has to be
--reversed.
decompressEntry abbreviation table = reverse $ decompressEntry' abbreviation where
  --Takes an abbreviation and expands it into a string.  
    decompressEntry'::Abbreviation->[Word8]
    --If the first entry is 0, nothing precedes the character.
    decompressEntry' (0,Just char)  = char:[]
    
    decompressEntry' (address,second)  = let
      --Look up the index is the table
      next = case (address `M.lookup` table) of
            Just abbrev->abbrev
            --If the index is not in the table then the file must be incorrectly constructed
            Nothing -> error "Invalid File!" in
       case second of
                                          Just char -> char:decompressEntry' next
                                          Nothing -> decompressEntry' next 

--The decoding table also starts out initialized with the 256 possible values
--of a byte.  It is the inverse of initEncodingTable
initDecodingTable::DecodingTable
--Convert initEncodingTable to a list, swap every tuple in that list, then
--convert the resulting list to a tuple.
initDecodingTable = M.fromList $ map swap $ M.toList initEncodingTable

--Takes a list of entries and expands it to a list of words.
decompressString:: [Abbreviation] -> [Word8]
decompressString abbreviations =  decompressString' abbreviations 256 initDecodingTable where
  --Takes a list of remaining entries, the index it is up to, and the current
  --state of the decodingTable and returns the expansion of the list of entries
  decompressString'::[Abbreviation] -> Index -> DecodingTable  -> [Word8]
  decompressString' [] _ _  = []
  --When the index wraps around to zero, reset the table to initDecodingTable
  decompressString' list 0 _  = decompressString' list 256 initDecodingTable
  --Expand the first abbreviation and append that to the beginning of a list
  --of expansion of the rest of the string.
  decompressString' (first:rest) index table  = (decompressEntry first table)
                                                ++ (decompressString' rest (index + 1) (M.insert index first table))


            {-backwards where
  (backwards,_,_) = foldl (\ (str,table,index) abbrev
                -> (decompressEntry abbrev table ++ str,M.insert index abbrev table, index + 1))
                 ([],M.empty,1) abbreviations-}

--blockLength::Int64
--blockLength = 2 * 2 ^ 16 - 1

--Takes a byteString representing an uncompressed file and returns a byteString
--representing a compressed file.
compressByteString::BS.ByteString -> BS.ByteString
compressByteString byteString = do
  let contents = BS.unpack byteString
  let compressed = compressString contents
  --Turn the list of abbreviations into a list of puts
  let asPut = map toPut compressed
  --Turn the list of puts into one long put and call runput on that
  runPut (sequence_ asPut) where
    toPut::Abbreviation -> Put
    --If the second entry is nothing then the put should consist of just the
    --index.  Otherwise, it should consist of the index followed by a byte.
    toPut (index, Nothing)   = putWord16le index
    toPut (index, Just word) = (putWord16le index) >> (putWord8 word)

--Returns a Get of an abbreviation
getAbbreviation::Get Abbreviation
getAbbreviation = 
  do
    address <- getWord16le
    --If the address is the last character of the file then return a get of
    --the address and nothing.
    empty <- isEmpty
    if empty then return (address,Nothing) else do
      character <- getWord8
      return (address,Just character)

--Constructs a Get to extract a list of abbreviations from a ByteString.
extractList::Get [Abbreviation]
extractList = do
  empty <- isEmpty
  --In the base case, return an empty list
  if empty then return [] else do
    --Extract the next abbreviation and the rest of the list and append the
    --next abbreviation to the beginning of the rest of the list.
    current <- getAbbreviation
    rest <- extractList
    return (current:rest)
{-extractList = fmap reverse $  extractList'  []  where
  extractList':: [Abbreviation] -> Get [Abbreviation]
  extractList' acc  = do
    empty <- isEmpty
    if empty then return acc else
      do
        abbr <- getAbbreviation
        extractList' (abbr:acc) -}

{-decompressByteString::BS.ByteString -> BS.ByteString
decompressByteString input = BS.append (BS.pack (decompressString (runGet extractList first)))
                             (if BS.null rest then BS.empty else decompressByteString rest) where
  (first,rest) = BS.splitAt blockLength input-}


main = do
  --The first argument is '-c' for compress or '-d' for decompress.  The
  --second argument is the name of the input file and the third argument is the
  --name of the output file.
    (option:inputfilename:outputfilename:_) <- getArgs
    content <- BS.readFile inputfilename
    case option of
     --Write the compressed content of the input file to the output file
          "-c" -> BS.writeFile outputfilename $ compressByteString content
          --Extract a list of abbreviations from content, decompress it, pack
          --the result and write it to the output file
          "-d" ->  BS.writeFile outputfilename $ BS.pack $ decompressString $ runGet extractList content
          _ -> putStr "Invalid option"
      
      
decodingTable::DecodingTable
decodingTable = M.fromList [(1,(0,Just 01)),(2,(0,Just 02)),(3,(1,Just 01)),(4,(1,Just 02)),(5,(3,Just 01)),(6,(4,Just 02))]

encodingTable::EncodingTable
encodingTable = M.fromList $ map swap $ M.toList  decodingTable

abbreviations::[Abbreviation]
abbreviations = [(0,Just 01),(0,Just 02),(1,Just 01),(1,Just 02),(3,Just 01),(4,Just 02)]

string::[Word8]
string = [01,02,01,01,01,02,01,01,01,01,02,02]
