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

        doc.add(Field.Text("number", (String) p.get("number")));
        doc.add(Field.Text("user", (String) p.get("user")));
        doc.add(Field.Text("date", (String) p.get("date")));
        doc.add(Field.Text("channel", (String) p.get("channel")));
        doc.add(Field.Text("title", (String) p.get("title")));
        doc.add(Field.Text("contents", (String) p.get("contents")));

        return doc;
    }
                
    public void indexPastes()
        throws InterruptedException, IOException {

        String indexFile = "pasteindex.luc";
        
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
            
            System.out.println("Indexing...");
            System.out.println(System.currentTimeMillis());
            for (int i = 0; i < pastes.size(); i++) {
                indexPaste(writer, i);
            }
            System.out.println(System.currentTimeMillis());

            System.out.println("\nOptimizing...");
            System.out.println(System.currentTimeMillis());
            writer.optimize();
            System.out.println(System.currentTimeMillis());
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
            PasteIndexer indexer = new PasteIndexer();
            System.out.println("Loading...");
            indexer.loadPastes(new File("../pastes.json"));
            System.out.println("Indexing " + indexer.pastes.size() + " pastes.");
            indexer.indexPastes();
        } catch (Exception e) {
            System.err.println("Got exception " + e);
            e.printStackTrace();
        }
    }
}