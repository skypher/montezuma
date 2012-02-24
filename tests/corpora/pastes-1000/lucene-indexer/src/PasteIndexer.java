import java.io.*;

import org.apache.lucene.analysis.*;
import org.apache.lucene.document.*;
import org.apache.lucene.index.*;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.json.simple.*;


public class PasteIndexer {

    protected JSONArray pastes;

    public void loadPastes(File file)
        throws java.io.FileNotFoundException {
        pastes = (JSONArray) JSONValue.parse(new FileReader(file));
    }

    public Document Document(int pasteNum) {
        JSONObject p = (JSONObject) pastes.get(pasteNum);
        Document doc = new Document();

        doc.add(new Field("id", (String) p.get("number"), true, true, false));
        doc.add(new Field("user", (String) p.get("user"), true, true, false));
        doc.add(new Field("date", (String) p.get("date"), true, true, false));
        doc.add(new Field("channel", (String) p.get("channel"), true, true, false));
        doc.add(new Field("title", (String) p.get("title"), true, true, true));
        doc.add(new Field("contents", (String) p.get("contents"), false, true, true));

        return doc;
    }
                
    public void indexPastes(String indexFile)
        throws InterruptedException, IOException {

        IndexWriter writer = null;
        File f;
        boolean create = true;
        // create index if the directory does not exist
        if ((f = new File(indexFile)).exists() && f.isDirectory()) {
            create = false;
        } else {
            create = true;
        }
        
        try {
            writer = new IndexWriter(indexFile, new StandardAnalyzer(), create);
            writer.minMergeDocs = 5000;
            writer.maxFieldLength = 1000000;
            
            long start, end;

            System.out.println("Indexing...");
            start = System.currentTimeMillis();
            for (int i = 0; i < pastes.size(); i++) {
                indexPaste(writer, i);
            }
            end = System.currentTimeMillis();
            System.out.println("Indexing took " + ((end - start) / 1000.0) + " seconds.");

            System.out.println("\nOptimizing...");
            start = System.currentTimeMillis();
            writer.optimize();
            end = System.currentTimeMillis();
            System.out.println("Optimizing took " + ((end - start) / 1000.0) + " seconds.");
        } finally {
            if (writer != null) 
                writer.close();
        }
    }

    public void indexPaste(IndexWriter writer, int pasteNum)
        throws IOException {
        writer.addDocument(Document(pasteNum));
    }

    public static void main (String args[]) {
        try{
            if (args.length != 2)
            {
                System.err.println("Usage: PasteIndexer <pastes.json> <pasteindex>");
                System.err.println("E.g., PasteIndexer ../pastes.json pasteindex.lucene");
                System.exit(1);
            }
            
            String pasteFile = args[0];
            String pasteIndexDir = args[1];

            PasteIndexer indexer = new PasteIndexer();
            System.out.println("Loading...");
            indexer.loadPastes(new File(pasteFile));
            System.out.println("Indexing " + indexer.pastes.size() + " pastes.");
            indexer.indexPastes(pasteIndexDir);

        } catch (Exception e) {
            System.err.println("Got exception " + e);
            e.printStackTrace();
        }
    }
}
